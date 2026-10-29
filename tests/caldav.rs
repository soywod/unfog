//! # CalDAV suite
//!
//! The GTD and time commands end to end against a CalDAV server: a local
//! Radicale (tests/radicale.sh) by default, a live provider through the
//! guard proxy (tests/live.sh) when `UNFOG_E2E_URL` names it. Ignored by
//! default: start the server, then run with `--ignored`.
//!
//! Every test works in calendars it creates for the run, named
//! `unfog-e2e-<run>-<n>`, and deletes them however it ends.

mod common;

use std::{
    fs,
    os::unix::fs::PermissionsExt,
    path::Path,
    process::Stdio,
    thread,
    time::{Duration, Instant},
};

use crate::common::{FOREIGN, MEETING, Server, Unfog, changed_lines};

/// The rows of a view, as `(uid, tasklist)`.
fn rows(view: &serde_json::Value) -> Vec<(String, String)> {
    view["tasks"]
        .as_array()
        .unwrap()
        .iter()
        .map(|row| {
            (
                row["uid"].as_str().unwrap().to_owned(),
                row["tasklist"].as_str().unwrap().to_owned(),
            )
        })
        .collect()
}

#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn a_capture_lands_in_the_inbox() {
    let _serial = common::serial();
    let server = Server::from_env();
    let tasks = server.scratch(&["VTODO"]);
    let dir = tempfile::tempdir().unwrap();
    let unfog = Unfog::new(dir.path(), &server, "");

    let captured = unfog.json(&["gtd", "capture", "-l", &tasks.id, "Call", "Bob"]);
    let uid = captured["uid"].as_str().unwrap().to_owned();

    let inbox = unfog.json(&["gtd", "inbox"]);
    assert!(
        rows(&inbox).contains(&(uid.clone(), tasks.id.clone())),
        "{inbox}"
    );

    let (_, body) = tasks
        .items()
        .into_iter()
        .find(|(_, body)| body.contains(&uid))
        .expect("the capture on the server");
    assert!(body.contains("CATEGORIES:inbox\r\n"), "{body}");
    assert!(body.contains("\r\nCREATED:"), "{body}");
}

/// A task another client wrote keeps every byte a clarification has no
/// business with: only `CATEGORIES` and `DTSTAMP` lines change, the
/// other `CATEGORIES` line, the folded summary, the parameters and the
/// X- property as the server served them.
#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn a_foreign_task_keeps_its_bytes() {
    let _serial = common::serial();
    let server = Server::from_env();
    let tasks = server.scratch(&["VTODO"]);
    let dir = tempfile::tempdir().unwrap();
    let unfog = Unfog::new(dir.path(), &server, "");

    let served = tasks.put("foreign.ics", FOREIGN);
    unfog.json(&["gtd", "clarify", "foreign-1", "--next", "-C", "phone"]);
    let after = tasks.get("foreign.ics").unwrap();

    let (removed, added) = changed_lines(&served, &after);
    let ours = |line: &String| line.starts_with("CATEGORIES") || line.starts_with("DTSTAMP");
    assert!(removed.iter().all(ours), "removed {removed:?}\n{after}");
    assert!(added.iter().all(ours), "added {added:?}\n{after}");
    assert!(after.contains("CATEGORIES:phone-ish\r\n"), "{after}");
    assert!(!after.to_ascii_lowercase().contains("inbox"), "{after}");

    let next = unfog.json(&["gtd", "next", "-C", "phone"]);
    assert!(
        rows(&next)
            .iter()
            .any(|(uid, _)| uid == "foreign-1@other.example"),
        "{next}"
    );
}

/// An edit that took a while does not overwrite a write another client
/// made meanwhile: the composer below changes the task on the server
/// while unfog waits for it, and the update is refused.
#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn a_stale_write_is_refused() {
    let _serial = common::serial();
    let server = Server::from_env();
    let tasks = server.scratch(&["VTODO"]);
    let dir = tempfile::tempdir().unwrap();
    let unfog = Unfog::new(dir.path(), &server, "");
    tasks.put("foreign.ics", FOREIGN);

    let ready = dir.path().join("ready");
    let go = dir.path().join("go");
    let composer = dir.path().join("composer.sh");
    fs::write(
        &composer,
        format!(
            "#!/bin/sh\ntouch '{}'\nwhile [ ! -e '{}' ]; do sleep 0.05; done\n\
             sed -i 's/^PRIORITY:5/PRIORITY:1/' \"$1\"\n",
            ready.display(),
            go.display()
        ),
    )
    .unwrap();
    fs::set_permissions(&composer, fs::Permissions::from_mode(0o700)).unwrap();

    let child = std::process::Command::new(env!("CARGO_BIN_EXE_unfog"))
        .arg("-c")
        .arg(&unfog.config)
        .args(["-b", "caldav", "task", "update", "-i", "--composer"])
        .arg(&composer)
        .args(["-l", &tasks.id, "foreign-1"])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    wait_for(&ready);
    let theirs = tasks.put(
        "foreign.ics",
        &FOREIGN.replace("SUMMARY;LANGUAGE=en:A", "SUMMARY;LANGUAGE=en:Their"),
    );
    fs::write(&go, "").unwrap();

    let output = child.wait_with_output().unwrap();
    let said = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(!output.status.success(), "{said}");
    assert!(
        said.contains("412") || said.to_lowercase().contains("precondition"),
        "{said}"
    );
    assert_eq!(tasks.get("foreign.ics").unwrap(), theirs);
}

/// Waits for `path` to exist, ten seconds at most.
fn wait_for(path: &Path) {
    let start = Instant::now();
    while !path.exists() {
        assert!(
            start.elapsed() < Duration::from_secs(10),
            "{} never came",
            path.display()
        );
        thread::sleep(Duration::from_millis(20));
    }
}

/// A move creates the task in the target, then removes it from the
/// source. A server refusing the same UID in two calendars of one home
/// refuses the create, which leaves the task where it was.
#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn a_move_creates_there_then_removes_here() {
    let _serial = common::serial();
    let server = Server::from_env();
    let from = server.scratch(&["VTODO"]);
    let to = server.scratch(&["VTODO"]);
    let dir = tempfile::tempdir().unwrap();
    let unfog = Unfog::new(dir.path(), &server, "");

    let served = from.put("foreign.ics", FOREIGN);
    let output = unfog.run(
        true,
        &["gtd", "clarify", "foreign-1", "--next", "--to", &to.id],
    );
    let said = String::from_utf8_lossy(&output.stdout).into_owned();

    if !output.status.success() {
        // NOTE: a server holding one UID per calendar home (Fastmail)
        // refuses the copy: the move fails whole, the task where it was
        // and nothing in the target. Printed for the record.
        assert!(said.contains("same UID"), "{said}");
        assert_eq!(from.get("foreign.ics").unwrap(), served, "{said}");
        assert!(to.items().is_empty(), "{said}");
        eprintln!("move: refused, the task left where it was: {said}");
        return;
    }

    assert!(from.items().is_empty(), "the source still holds it");
    let moved = to.items();
    assert_eq!(moved.len(), 1);
    assert!(
        moved[0].1.contains("UID:foreign-1@other.example\r\n"),
        "{}",
        moved[0].1
    );

    let next = unfog.json(&["gtd", "next"]);
    assert!(
        rows(&next).contains(&("foreign-1@other.example".into(), to.id.clone())),
        "{next}"
    );
}

/// The time strategies writing to a calendar: a running entry waits on
/// the device, a stop writes it finished there, and a meeting sharing
/// the calendar is never touched.
fn a_strategy_round_trips(strategy: &str, component: &str) {
    let _serial = common::serial();
    let server = Server::from_env();
    let tasks = server.scratch(&["VTODO"]);
    let time = server.scratch(&[component]);
    let dir = tempfile::tempdir().unwrap();
    let unfog = Unfog::new(
        dir.path(),
        &server,
        &format!(
            "time.strategy = \"{strategy}\"\ntime.collection = \"{}\"\n",
            time.id
        ),
    );

    tasks.put("foreign.ics", FOREIGN);
    let meeting = match component {
        "VEVENT" => Some(time.put("meeting.ics", MEETING)),
        _ => None,
    };

    unfog.json(&["task", "start", "foreign-1"]);
    let status = unfog.json(&["task", "status"]);
    assert_eq!(status["running"].as_array().unwrap().len(), 1, "{status}");
    assert_eq!(
        time.items().len(),
        usize::from(meeting.is_some()),
        "a running entry left the device"
    );

    thread::sleep(Duration::from_millis(1100));
    let stopped = unfog.json(&["task", "stop"]);
    assert_eq!(stopped["stopped"].as_array().unwrap().len(), 1, "{stopped}");

    let entries: Vec<String> = time
        .items()
        .into_iter()
        .map(|(_, body)| body)
        .filter(|body| body.contains("RELATED-TO:foreign-1@other.example"))
        .collect();

    match component {
        "VEVENT" => {
            assert_eq!(entries.len(), 1, "{entries:#?}");
            assert!(entries[0].contains("\r\nDTEND:"), "{}", entries[0]);
            assert!(
                entries[0].contains("TRANSP:TRANSPARENT\r\n"),
                "{}",
                entries[0]
            );
        }
        _ => {
            assert_eq!(entries.len(), 2, "{entries:#?}");
            assert!(
                entries
                    .iter()
                    .any(|body| body.contains("CATEGORIES:start\r\n"))
            );
            assert!(
                entries
                    .iter()
                    .any(|body| body.contains("CATEGORIES:stop\r\n"))
            );
        }
    }

    if let Some(meeting) = meeting {
        assert_eq!(time.get("meeting.ics").unwrap(), meeting);
    }

    let status = unfog.json(&["task", "status"]);
    assert!(status["running"].as_array().unwrap().is_empty(), "{status}");
    let worktime = unfog.json(&["worktime"]);
    assert!(worktime["seconds"].as_i64().unwrap() >= 1, "{worktime}");
}

#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn the_event_strategy_round_trips() {
    a_strategy_round_trips("event", "VEVENT");
}

#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn the_journal_strategy_round_trips() {
    a_strategy_round_trips("journal", "VJOURNAL");
}

/// Removes the scratch collections of every run, run last by
/// tests/live.sh so a run cut short leaves nothing behind.
#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn zz_the_leftovers_are_swept() {
    let _serial = common::serial();
    let server = Server::from_env();
    server.sweep();
}

/// What the server takes: a scratch calendar asked for each component
/// alone, and the component set it then states. Printed for the record,
/// failing only when no task calendar can be made.
#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn the_server_states_the_components_it_takes() {
    let _serial = common::serial();
    let server = Server::from_env();
    let mut tasks = false;

    for component in ["VTODO", "VEVENT", "VJOURNAL"] {
        match server.try_scratch(&[component]) {
            Ok(scratch) => {
                let stated = server.http(
                    "PROPFIND",
                    &scratch.path,
                    &[("Depth", "0"), ("Content-Type", "application/xml")],
                    "<?xml version=\"1.0\"?><D:propfind xmlns:D=\"DAV:\" \
                     xmlns:C=\"urn:ietf:params:xml:ns:caldav\"><D:prop>\
                     <C:supported-calendar-component-set/></D:prop></D:propfind>",
                );
                let names: Vec<&str> = stated
                    .body
                    .split("name=\"")
                    .skip(1)
                    .filter_map(|rest| rest.split('"').next())
                    .collect();
                eprintln!("components: {component} created, stated {names:?}");
                tasks |= component == "VTODO";
            }
            Err(response) => {
                eprintln!(
                    "components: {component} refused {} {}",
                    response.status,
                    response.body.trim()
                );
            }
        }
    }

    assert!(tasks, "no task calendar could be made");
}

/// A capture into a calendar stating it takes no tasks is refused before
/// anything is sent, saying why (Google answers a bare 403).
#[test]
#[ignore = "requires a CalDAV server (tests/radicale.sh or tests/live.sh) and --ignored"]
fn a_capture_into_an_events_calendar_says_why() {
    let _serial = common::serial();
    let server = Server::from_env();
    let events = server.scratch(&["VEVENT"]);
    let dir = tempfile::tempdir().unwrap();
    let unfog = Unfog::new(dir.path(), &server, "");

    let output = unfog.run(true, &["gtd", "capture", "-l", &events.id, "Call", "Bob"]);
    let said = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(!output.status.success(), "{said}");
    assert!(said.contains("takes no tasks"), "{said}");
    assert!(events.items().is_empty());
}
