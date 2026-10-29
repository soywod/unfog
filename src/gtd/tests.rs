//! The GTD verbs over offline fixtures: vdir homes and pimdir stores in
//! temporary directories.

use clap::Parser;
use jiff::Timestamp;

use crate::{
    gtd::{
        capture::capture,
        clarify::ClarifyCommand,
        review::ReviewCommand,
        view::{InboxCommand, NextCommand, SomedayCommand, WaitingCommand, view},
    },
    scope::Scope,
    shared::task::gtd::GtdView,
    testing::{Kept, config, gtd_fixture, scope, task, uids},
};

fn at() -> Timestamp {
    "2026-10-28T08:30:00Z".parse().unwrap()
}

/// The `uid`s of a view over a scope.
fn listed(scope: &mut Scope, kind: GtdView, filter: &[&str]) -> Vec<String> {
    let filter: Vec<String> = filter.iter().map(|value| value.to_string()).collect();
    let output = view(scope, kind, &filter, None).unwrap();
    uids(&serde_json::to_value(output).unwrap(), "tasks", "uid")
}

/// Each view of the fixture, whatever the backend holding it.
fn the_views_hold(scope: &mut Scope) {
    assert_eq!(listed(scope, GtdView::Inbox, &[]), ["inbox-1"]);
    assert_eq!(listed(scope, GtdView::Next, &[]), ["next-1", "next-2"]);
    assert_eq!(listed(scope, GtdView::Next, &["WORK"]), ["next-2"]);
    assert_eq!(listed(scope, GtdView::Waiting, &[]), ["waiting-1"]);
    assert_eq!(listed(scope, GtdView::Someday, &[]), ["someday-1"]);
    assert!(listed(scope, GtdView::Next, &["nowhere"]).is_empty());
}

#[cfg(feature = "vdir")]
mod vdir {
    use super::*;
    use crate::testing::{vdir_account, vdir_items};

    fn fixture(dir: &tempfile::TempDir) -> crate::config::Config {
        let home = dir.path().join("home");
        let account = vdir_account(&home, &["tasks", "work"]);
        vdir_items(&home, "tasks", &gtd_fixture());
        config(vec![("personal", account)])
    }

    /// A task is found by its handle or a prefix of its `UID`, dashes
    /// ignored, the handle winning; a prefix naming several is refused
    /// naming each, one naming none as not found.
    #[test]
    fn a_task_is_found_across_the_scope() {
        let dir = tempfile::tempdir().unwrap();
        let home = dir.path().join("home");
        let account = vdir_account(&home, &["tasks", "work"]);
        vdir_items(
            &home,
            "tasks",
            &[
                task("4c1e0b2a-7d3e-4f00-9b1a-000000000001", ""),
                task("4c1e77d0-0000-4000-8000-000000000002", ""),
            ],
        );
        vdir_items(
            &home,
            "work",
            &[task("9f00aa11-0000-4000-8000-000000000003", "")],
        );
        let mut scope = scope(&config(vec![("personal", account)]));

        assert_eq!(scope.find("9f00").unwrap().tasklist, "work");
        assert_eq!(
            scope.find("4c1e-0b2a7d").unwrap().task.uid,
            "4c1e0b2a-7d3e-4f00-9b1a-000000000001"
        );

        let err = scope.find("4c1e").unwrap_err().to_string();
        assert!(err.contains("4c1e0b2a-7d3e"), "{err}");
        assert!(err.contains("4c1e77d0-0000"), "{err}");

        assert!(
            scope
                .find("zz")
                .unwrap_err()
                .to_string()
                .contains("Cannot find")
        );
        assert!(
            scope
                .find_in("9f00", Some("tasks"))
                .unwrap_err()
                .to_string()
                .contains("Cannot find")
        );

        // NOTE: a handle names its task, `-l` telling apart two tasklists
        // using the same one.
        let handle = scope.find("9f00").unwrap().task.id;
        assert_eq!(
            scope.find_in(&handle, Some("work")).unwrap().tasklist,
            "work"
        );
    }

    /// The file the task of `uid` is stored in, read back.
    fn stored(dir: &tempfile::TempDir, tasklist: &str, uid: &str) -> Option<String> {
        std::fs::read_dir(dir.path().join("home").join(tasklist))
            .unwrap()
            .map(|entry| std::fs::read_to_string(entry.unwrap().path()).unwrap())
            .find(|body| body.contains(&format!("UID:{uid}\r\n")))
    }

    #[test]
    fn each_view_holds_its_tasks_on_vdir() {
        let dir = tempfile::tempdir().unwrap();
        the_views_hold(&mut scope(&fixture(&dir)));
    }

    #[test]
    fn the_view_commands_print_the_view_and_its_query() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let mut printer = Kept::default();

        InboxCommand::try_parse_from(["inbox"])
            .unwrap()
            .execute(&mut printer, scope(&config))
            .unwrap();
        assert_eq!(printer.last()["view"], "inbox");
        assert_eq!(
            printer.last()["query"],
            "kind:task -status:completed -status:cancelled category:inbox"
        );
        assert_eq!(printer.last()["tasks"][0]["account"], "personal");
        assert_eq!(printer.last()["tasks"][0]["tasklist"], "tasks");
        // NOTE: counted over the scope, where `next-1` and `next-2` take
        // five characters to tell apart.
        assert_eq!(printer.last()["tasks"][0]["shortId"], "inbox");

        NextCommand::try_parse_from(["next", "-C", "home"])
            .unwrap()
            .execute(&mut printer, scope(&config))
            .unwrap();
        assert_eq!(uids(printer.last(), "tasks", "uid"), ["next-1"]);

        WaitingCommand::try_parse_from(["waiting"])
            .unwrap()
            .execute(&mut printer, scope(&config))
            .unwrap();
        assert_eq!(uids(printer.last(), "tasks", "uid"), ["waiting-1"]);

        SomedayCommand::try_parse_from(["someday"])
            .unwrap()
            .execute(&mut printer, scope(&config))
            .unwrap();
        assert_eq!(uids(printer.last(), "tasks", "uid"), ["someday-1"]);
    }

    #[test]
    fn the_views_span_every_account() {
        let dir = tempfile::tempdir().unwrap();
        let other = dir.path().join("other");
        let mut config = fixture(&dir);
        config
            .accounts
            .insert("work".into(), vdir_account(&other, &["inbox"]));
        vdir_items(
            &other,
            "inbox",
            &[task(
                "inbox-2",
                "SUMMARY:Reply to Ann\r\nCATEGORIES:inbox\r\n",
            )],
        );

        let mut scope = scope(&config);
        assert_eq!(
            listed(&mut scope, GtdView::Inbox, &[]),
            ["inbox-1", "inbox-2"]
        );

        // NOTE: `-a` narrows the scope to one account.
        let mut one = Scope::new(config, Some("work"), crate::backend::Backend::Auto).unwrap();
        assert_eq!(listed(&mut one, GtdView::Inbox, &[]), ["inbox-2"]);
    }

    #[test]
    fn the_review_counts_every_view_first() {
        let dir = tempfile::tempdir().unwrap();
        let mut printer = Kept::default();

        ReviewCommand::try_parse_from(["review"])
            .unwrap()
            .execute(&mut printer, scope(&fixture(&dir)))
            .unwrap();

        let counts = &printer.last()["counts"];
        assert_eq!(
            (
                &counts["inbox"],
                &counts["next"],
                &counts["waiting"],
                &counts["someday"]
            ),
            (&1.into(), &2.into(), &1.into(), &1.into())
        );
    }

    /// Waiting shows who a task waits on and for how long, and the
    /// review lists the tasks waiting over two weeks.
    #[test]
    fn waiting_shows_the_contact_and_the_wait() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        vdir_items(
            &dir.path().join("home"),
            "work",
            &[task(
                "waiting-2",
                "SUMMARY:Signed lease\r\nCATEGORIES:waiting\r\nCONTACT:Jo Smith\r\n",
            )],
        );
        let now: Timestamp = "2026-10-08T08:00:00Z".parse().unwrap();

        let output =
            crate::gtd::view::view_at(&mut scope(&config), GtdView::Waiting, &[], None, now)
                .unwrap();
        let rows: Vec<(String, Vec<String>, Option<i64>)> = output
            .tasks
            .iter()
            .map(|row| {
                (
                    row.task.uid.clone(),
                    row.task.contacts.clone(),
                    row.waited_days,
                )
            })
            .collect();
        assert!(rows.contains(&("waiting-2".into(), vec!["Jo Smith".into()], Some(7))));
        assert!(format!("{output}").contains("Jo Smith"), "{output}");

        let mut printer = Kept::default();
        ReviewCommand::try_parse_from(["review"])
            .unwrap()
            .execute_at(
                &mut printer,
                scope(&config),
                "2026-10-20T08:00:00Z".parse().unwrap(),
            )
            .unwrap();
        let long = uids(printer.last(), "waitingLong", "uid");
        assert_eq!(long, ["waiting-1", "waiting-2"]);
        assert!(printer.last()["replied"].as_array().unwrap().is_empty());
    }

    /// A message piped to a capture gives its subject and a link back to
    /// it, and every capture says when it was captured.
    #[test]
    fn a_captured_message_links_back_to_it() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let client = scope(&config).accounts.remove(0).client;
        let mail = "From: Jo <jo@example.org>\nSubject: Lease renewal\n\
                    Message-ID: <lease/42@example.org>\n\nPlease sign.\n";

        let captured =
            crate::gtd::capture::capture_at(client, Some("tasks".into()), mail, at()).unwrap();
        let body = stored(&dir, "tasks", &captured.uid).unwrap();

        assert!(body.contains("SUMMARY:Lease renewal\r\n"), "{body}");
        assert!(
            body.contains("DTSTAMP:20261028T083000Z\r\nCREATED:20261028T083000Z\r\n"),
            "{body}"
        );
        assert!(
            body.contains("ATTACH:mid:lease%2F42@example.org\r\n"),
            "{body}"
        );
        assert!(!body.contains("Please sign"), "{body}");

        let found = scope(&config).find(&captured.uid).unwrap();
        assert_eq!(found.task.mid.as_deref(), Some("lease/42@example.org"));
    }

    #[test]
    fn a_capture_lands_in_the_inbox_of_the_capture_tasklist() {
        let dir = tempfile::tempdir().unwrap();
        let mut config = fixture(&dir);
        config
            .accounts
            .get_mut("personal")
            .unwrap()
            .capture
            .tasklist = Some("work".into());

        let mut scope_ = scope(&config);
        let client = scope_.accounts.remove(0).client;
        let captured = capture(client, None, "Buy oat milk\nthe barista one\n").unwrap();

        assert_eq!(captured.tasklist, "work");
        let body = stored(&dir, "work", &captured.uid).unwrap();
        assert!(body.contains("SUMMARY:Buy oat milk\r\n"), "{body}");
        assert!(body.contains("DESCRIPTION:the barista one\r\n"), "{body}");
        assert!(body.contains("CATEGORIES:inbox\r\n"), "{body}");

        let mut inbox = listed(&mut scope(&config), GtdView::Inbox, &[]);
        inbox.retain(|uid| *uid != "inbox-1");
        assert_eq!(inbox, [captured.uid]);
    }

    #[test]
    fn a_task_already_in_the_state_asked_is_left_untouched() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let before = stored(&dir, "tasks", "someday-1").unwrap();

        let mut printer = Kept::default();
        ClarifyCommand::try_parse_from(["clarify", "someday-1", "--someday"])
            .unwrap()
            .execute_at(&mut printer, scope(&config), at())
            .unwrap();

        assert_eq!(printer.last()["clarified"][0]["unchanged"], true);
        assert_eq!(stored(&dir, "tasks", "someday-1").unwrap(), before);

        // NOTE: a next action given a category is changed.
        let before = stored(&dir, "tasks", "next-1").unwrap();
        ClarifyCommand::try_parse_from(["clarify", "next-1", "--next"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();
        assert_eq!(stored(&dir, "tasks", "next-1").unwrap(), before);
        ClarifyCommand::try_parse_from(["clarify", "next-1", "--next", "-C", "paint"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();
        assert_ne!(stored(&dir, "tasks", "next-1").unwrap(), before);
    }

    /// A clarified next action can name the task it is a step of.
    #[test]
    fn a_next_action_names_its_parent() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);

        ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "--parent", "next-2"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();

        let body = stored(&dir, "tasks", "inbox-1").unwrap();
        assert!(
            body.contains("RELATED-TO;RELTYPE=PARENT:next-2\r\n")
                || body.contains("RELATED-TO:next-2\r\n"),
            "{body}"
        );
        assert_eq!(
            scope(&config)
                .find("inbox-1")
                .unwrap()
                .task
                .parent
                .as_deref(),
            Some("next-2")
        );
    }

    /// A recurring task is clarified on its master, its overrides kept.
    #[test]
    fn a_recurring_task_is_clarified_on_its_master() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let series = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//other//client//EN\r\n\
            BEGIN:VTODO\r\nUID:series-1\r\nDTSTAMP:20261001T080000Z\r\n\
            DTSTART:20261005T090000Z\r\nRRULE:FREQ=WEEKLY\r\nSUMMARY:Water the plants\r\n\
            CATEGORIES:inbox\r\nEND:VTODO\r\n\
            BEGIN:VTODO\r\nUID:series-1\r\nDTSTAMP:20261001T080000Z\r\n\
            RECURRENCE-ID:20261012T090000Z\r\nDTSTART:20261013T090000Z\r\n\
            SUMMARY:Water the plants (late)\r\nCATEGORIES:inbox\r\nEND:VTODO\r\n\
            END:VCALENDAR\r\n";
        std::fs::write(dir.path().join("home/tasks/series.ics"), series).unwrap();

        ClarifyCommand::try_parse_from(["clarify", "series-1", "--someday"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();

        let body = std::fs::read_to_string(dir.path().join("home/tasks/series.ics")).unwrap();
        let (master, overriding) = body.split_once("END:VTODO\r\n").unwrap();
        assert!(master.contains("CATEGORIES:someday\r\n"), "{body}");
        assert!(overriding.contains("CATEGORIES:inbox\r\n"), "{body}");
        assert!(
            overriding.contains("SUMMARY:Water the plants (late)\r\n"),
            "{body}"
        );
    }

    #[test]
    fn clarifying_as_a_next_action_edits_only_the_reserved_category() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let home = dir.path().join("home");
        let original = task(
            "inbox-9",
            "SUMMARY:Call Jo\r\nCATEGORIES:work,Inbox,q3\r\nCATEGORIES:phone-ish\r\n",
        );
        std::fs::write(home.join("tasks/inbox-9.ics"), &original).unwrap();

        ClarifyCommand::try_parse_from(["clarify", "inbox-9", "--next", "-C", "phone"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();

        assert_eq!(
            stored(&dir, "tasks", "inbox-9").unwrap(),
            original
                .replace("DTSTAMP:20261001T080000Z", "DTSTAMP:20261028T083000Z")
                .replace(
                    "CATEGORIES:work,Inbox,q3\r\nCATEGORIES:phone-ish\r\n",
                    "CATEGORIES:work,q3\r\nCATEGORIES:phone-ish\r\nCATEGORIES:phone\r\n"
                )
        );
        assert!(listed(&mut scope(&config), GtdView::Next, &["phone"]).contains(&"inbox-9".into()));
    }

    #[test]
    fn clarifying_as_waiting_names_the_contact_and_rephrases() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);

        ClarifyCommand::try_parse_from([
            "clarify",
            "inbox-1",
            "--summary",
            "Call Jo about the lease",
            "--waiting",
            "--contact",
            "Jo",
        ])
        .unwrap()
        .execute_at(&mut Kept::default(), scope(&config), at())
        .unwrap();

        let body = stored(&dir, "tasks", "inbox-1").unwrap();
        assert!(
            body.contains("SUMMARY:Call Jo about the lease\r\n"),
            "{body}"
        );
        assert!(body.contains("CATEGORIES:waiting\r\n"), "{body}");
        assert!(body.contains("CONTACT:Jo\r\n"), "{body}");
        assert!(body.contains("X-OTHER-CLIENT:kept\r\n"), "{body}");
        assert_eq!(
            listed(&mut scope(&config), GtdView::Waiting, &[]),
            ["inbox-1", "waiting-1"]
        );
    }

    #[test]
    fn clarifying_with_a_priority_shows_it_in_the_view() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);

        ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "--priority", "2"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();

        let body = stored(&dir, "tasks", "inbox-1").unwrap();
        assert!(body.contains("PRIORITY:2\r\n"), "{body}");

        let output = view(&mut scope(&config), GtdView::Next, &[], None).unwrap();
        let rendered = output.to_string();
        assert!(rendered.contains("PRIORITY"), "{rendered}");

        let json = serde_json::to_value(output).unwrap();
        let task = json["tasks"]
            .as_array()
            .unwrap()
            .iter()
            .find(|task| task["uid"] == "inbox-1")
            .unwrap();
        assert_eq!(task["priority"], 2);
        assert!(
            ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "--priority", "10"])
                .is_err()
        );
    }

    #[test]
    fn clarifying_as_done_or_deleted_leaves_every_view() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let mut printer = Kept::default();

        ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--done"])
            .unwrap()
            .execute_at(&mut printer, scope(&config), at())
            .unwrap();
        let body = stored(&dir, "tasks", "inbox-1").unwrap();
        assert!(body.contains("STATUS:COMPLETED\r\n"), "{body}");
        assert!(!body.contains("CATEGORIES"), "{body}");
        assert_eq!(printer.last()["clarified"][0]["outcome"], "done");

        ClarifyCommand::try_parse_from(["clarify", "someday-1", "--delete"])
            .unwrap()
            .execute_at(&mut printer, scope(&config), at())
            .unwrap();
        assert!(stored(&dir, "tasks", "someday-1").is_none());
        assert!(listed(&mut scope(&config), GtdView::Someday, &[]).is_empty());
    }

    #[test]
    fn clarifying_into_another_tasklist_creates_there_then_removes_here() {
        let dir = tempfile::tempdir().unwrap();
        let config = fixture(&dir);
        let mut printer = Kept::default();

        ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--someday", "--to", "work"])
            .unwrap()
            .execute_at(&mut printer, scope(&config), at())
            .unwrap();

        assert!(stored(&dir, "tasks", "inbox-1").is_none());
        let moved = stored(&dir, "work", "inbox-1").unwrap();
        assert!(moved.contains("CATEGORIES:someday\r\n"), "{moved}");
        assert_eq!(printer.last()["clarified"][0]["moved"], true);
        assert_eq!(printer.last()["clarified"][0]["tasklist"], "work");
    }

    #[test]
    fn the_flags_refuse_a_move_with_no_task_to_move() {
        let dir = tempfile::tempdir().unwrap();
        let err = ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--done", "--to", "work"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&fixture(&dir)), at())
            .unwrap_err();
        assert!(err.to_string().contains("moves nowhere"), "{err}");
        assert!(stored(&dir, "tasks", "inbox-1").is_some());

        assert!(ClarifyCommand::try_parse_from(["clarify", "x", "--done", "--next"]).is_err());
        let err =
            ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "--contact", "Jo"])
                .unwrap()
                .execute_at(&mut Kept::default(), scope(&fixture(&dir)), at())
                .unwrap_err();
        assert!(err.to_string().contains("--waiting"), "{err}");
    }
}

#[cfg(feature = "pimdir")]
mod pimdir {
    use super::*;
    use crate::{
        shared::item::TaskItemQuery,
        testing::{drain, pimdir_account},
    };

    #[test]
    fn each_view_holds_its_tasks_on_pimdir() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal"], &gtd_fixture());
        the_views_hold(&mut scope(&config(vec![("personal", account)])));
    }

    /// A staged edit shows in the views before a sync applies it: the
    /// summary beside the row still describes the old body.
    #[test]
    fn a_staged_clarification_moves_the_task_between_views_at_once() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal"], &gtd_fixture());
        let config = config(vec![("personal", account)]);

        ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "-C", "phone"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();

        let mut scope = scope(&config);
        assert!(listed(&mut scope, GtdView::Inbox, &[]).is_empty());
        assert_eq!(listed(&mut scope, GtdView::Next, &["Phone"]), ["inbox-1"]);
    }

    /// A capture staged as an add shows in the Inbox before a sync, and
    /// a clarification of it waits for the sync, refused with its code.
    #[test]
    fn a_staged_capture_shows_at_once_and_waits_to_be_edited() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal"], &gtd_fixture());
        let config = config(vec![("personal", account)]);

        let client = scope(&config).accounts.remove(0).client;
        let captured = capture(client, Some("cal".into()), "Call Bob").unwrap();

        let mut expected = vec!["inbox-1".to_owned(), captured.uid.clone()];
        expected.sort();
        assert_eq!(listed(&mut scope(&config), GtdView::Inbox, &[]), expected);

        let err = ClarifyCommand::try_parse_from(["clarify", &captured.uid, "--next"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap_err();
        assert_eq!(
            pimalaya_cli::error::code_of::<crate::error::ErrorCode>(&err),
            Some(crate::error::ErrorCode::PendingAdd),
            "{err:#}"
        );

        drain(dir.path());
        ClarifyCommand::try_parse_from(["clarify", &captured.uid, "--next"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();
        assert!(listed(&mut scope(&config), GtdView::Next, &[]).contains(&captured.uid));
    }

    /// A calendar its source states refuses VTODO is no tasklist, and a
    /// capture into it is refused before it is queued.
    #[test]
    fn a_calendar_refusing_tasks_is_no_tasklist() {
        use io_pimdir::capability::{CALENDAR, PimdirCapability, PimdirSupport, X_VTODO};

        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal", "events"], &gtd_fixture());
        let config = config(vec![("personal", account)]);

        let event = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\nBEGIN:VEVENT\r\n\
            UID:e1\r\nDTSTAMP:20261001T080000Z\r\nDTSTART:20261005T090000Z\r\nEND:VEVENT\r\n\
            END:VCALENDAR\r\n";
        let mut client = scope(&config).accounts.remove(0).client;
        client
            .create_item("events", event.as_bytes().to_vec())
            .unwrap();
        drop(client);
        drain(dir.path());

        // NOTE: every calendar capability stated, and VTODO refused in
        // `events` alone.
        let mut declared: Vec<PimdirCapability> = CALENDAR
            .iter()
            .map(|name| PimdirCapability {
                collection: None,
                name: name.to_string(),
                support: PimdirSupport::Full,
                detail: None,
            })
            .collect();
        declared.push(PimdirCapability {
            collection: Some("events".into()),
            name: X_VTODO.into(),
            support: PimdirSupport::None,
            detail: Some("VEVENT only".into()),
        });
        let mut store = io_pimdir::client::PimdirStore::open(dir.path()).unwrap();
        store.declare("caldav", &declared).unwrap();
        drop(store);

        let mut client = scope(&config).accounts.remove(0).client;
        let ids: Vec<String> = client
            .list_tasklists()
            .unwrap()
            .into_iter()
            .map(|tasklist| tasklist.id)
            .collect();
        assert_eq!(ids, ["cal"]);

        let err = capture(client, Some("events".into()), "Call Bob").unwrap_err();
        assert!(format!("{err:#}").contains("x-vtodo"), "{err:#}");
    }

    /// The review names the waiting tasks whose mail got a reply.
    #[test]
    fn the_review_names_a_waiting_task_whose_mail_got_a_reply() {
        use io_pimdir::{
            change::PimdirWriteOp,
            collection::PimdirCollectionId,
            placement::{
                PimdirBase, PimdirFlags, PimdirHandle, PimdirLevel, PimdirLinkId, PimdirPlacement,
                PimdirStatus,
            },
            summary::{PimdirSummary, mail::PimdirMailSummary},
        };

        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(
            dir.path(),
            &["cal"],
            &[
                task(
                    "asked-1",
                    "SUMMARY:Lease\r\nCATEGORIES:waiting\r\nATTACH:mid:asked%40example.org\r\n",
                ),
                task(
                    "asked-2",
                    "SUMMARY:Quote\r\nCATEGORIES:waiting\r\nATTACH:mid:silent@example.org\r\n",
                ),
            ],
        );
        let config = config(vec![("personal", account)]);

        let mut store = io_pimdir::client::PimdirStore::open(dir.path())
            .unwrap()
            .for_source("imap");
        store.ensure_collection("INBOX", "message/rfc822").unwrap();
        store
            .write(vec![PimdirWriteOp::UpsertPlacement(PimdirPlacement {
                sort_key: Default::default(),
                collection: PimdirCollectionId("INBOX".into()),
                handle: PimdirHandle("1".into()),
                link_id: Some(PimdirLinkId("r1@example.org".into())),
                object: None,
                level: PimdirLevel::Meta,
                summary: Some(PimdirSummary::Mail(PimdirMailSummary {
                    message_id: Some("r1@example.org".into()),
                    in_reply_to: vec!["asked@example.org".into()],
                    subject: "Re: the lease".into(),
                    ..Default::default()
                })),
                flags: PimdirFlags::default(),
                status: PimdirStatus::Clean,
                conflict_revision: None,
                conflict_object: None,
                base: Some(PimdirBase {
                    flags: PimdirFlags::default(),
                    revision: None,
                    object: None,
                }),
                origin: None,
            })])
            .unwrap();
        drop(store);

        let mut printer = Kept::default();
        ReviewCommand::try_parse_from(["review"])
            .unwrap()
            .execute_at(&mut printer, scope(&config), at())
            .unwrap();
        assert_eq!(uids(printer.last(), "replied", "uid"), ["asked-1"]);
    }

    /// A move staged by `clarify --to` shows the task where it goes, once,
    /// before a sync.
    #[test]
    fn a_staged_move_shows_the_task_once_where_it_goes() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal", "other"], &gtd_fixture());
        let config = config(vec![("personal", account)]);

        ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "--to", "other"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap();

        let output = view(&mut scope(&config), GtdView::Next, &[], None).unwrap();
        let rows: Vec<(String, String)> = output
            .tasks
            .iter()
            .filter(|row| row.task.uid == "inbox-1")
            .map(|row| (row.tasklist.clone(), row.task.uid.clone()))
            .collect();
        assert_eq!(rows, [("other".to_owned(), "inbox-1".to_owned())]);
        assert!(listed(&mut scope(&config), GtdView::Inbox, &[]).is_empty());

        // NOTE: one add superseding the source, no remove of its own
        // (pimdir STORAGE §15): the source goes only once the add did.
        let reader = io_pimdir::client::reader::PimdirReader::open(dir.path()).unwrap();
        let pending = reader.list_pending_actions().unwrap();
        assert_eq!(pending.len(), 1, "{pending:?}");
        assert!(matches!(
            &pending[0].action,
            io_pimdir::codec::PimdirAction::Add {
                supersedes: Some(superseded),
                ..
            } if superseded.collection.0 == "cal"
        ));
        drop(reader);

        drain(dir.path());
        let output = view(&mut scope(&config), GtdView::Next, &[], None).unwrap();
        let tasklists: Vec<&str> = output
            .tasks
            .iter()
            .filter(|row| row.task.uid == "inbox-1")
            .map(|row| row.tasklist.as_str())
            .collect();
        assert_eq!(tasklists, ["other"]);
    }

    /// The create is refused (the target is no collection of the store),
    /// so the capture stays where it was and nothing is staged.
    #[test]
    fn a_refused_create_keeps_the_capture() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal"], &gtd_fixture());
        let config = config(vec![("personal", account)]);

        let err = ClarifyCommand::try_parse_from(["clarify", "inbox-1", "--next", "--to", "gone"])
            .unwrap()
            .execute_at(&mut Kept::default(), scope(&config), at())
            .unwrap_err();
        assert!(format!("{err:#}").contains("left where it was"), "{err:#}");

        let reader = io_pimdir::client::reader::PimdirReader::open(dir.path()).unwrap();
        assert!(reader.list_pending_actions().unwrap().is_empty());
        drop(reader);
        drain(dir.path());

        assert_eq!(
            listed(&mut scope(&config), GtdView::Inbox, &[]),
            ["inbox-1"]
        );
    }

    /// The local time collection is a calendar under no account, and
    /// holds no task: `tasklist list` leaves it out.
    #[test]
    fn the_local_time_collection_is_no_tasklist() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal"], &[]);
        {
            let mut store = io_pimdir::client::PimdirStore::open(dir.path()).unwrap();
            store.ensure_time_collection().unwrap();
        }

        let mut client = scope(&config(vec![("personal", account)]))
            .accounts
            .remove(0)
            .client;
        let ids: Vec<String> = client
            .list_tasklists()
            .unwrap()
            .into_iter()
            .map(|tasklist| tasklist.id)
            .collect();
        assert_eq!(ids, ["cal"]);
        assert!(
            client
                .list_items("cal", TaskItemQuery::default())
                .unwrap()
                .is_empty()
        );
    }
}

/// The verbs run as the binary runs them: parsed from the command line,
/// over a configuration file.
#[cfg(feature = "vdir")]
mod cli {
    use std::{fs, path::Path};

    use clap::Parser;

    use crate::{
        cli::UnfogCli,
        testing::{Kept, task},
    };

    /// Runs `args` over the configuration at `config`.
    fn run(config: &Path, args: &[&str]) -> anyhow::Result<serde_json::Value> {
        let config = config.to_string_lossy().into_owned();
        let argv = [&["unfog", "-c", config.as_str()], args].concat();
        let cli = UnfogCli::try_parse_from(argv).unwrap();
        let mut printer = Kept::default();
        cli.command.unwrap().execute(
            &mut printer,
            &cli.config_paths,
            cli.account.name.as_deref(),
            cli.backend,
        )?;
        Ok(printer.outputs.last().cloned().unwrap_or_default())
    }

    /// A vdir account of two tasklists, `tasks` the default one, and the
    /// configuration naming it, plus `extra` accounts verbatim.
    fn home(dir: &Path, bodies: &[(&str, String)], extra: &str) -> std::path::PathBuf {
        for tasklist in ["tasks", "work"] {
            fs::create_dir_all(dir.join("home").join(tasklist)).unwrap();
        }
        for (index, (tasklist, body)) in bodies.iter().enumerate() {
            fs::write(
                dir.join("home").join(tasklist).join(format!("{index}.ics")),
                body,
            )
            .unwrap();
        }

        let config = dir.join("config.toml");
        fs::write(
            &config,
            format!(
                "[accounts.personal]\ndefault = true\nvdir.home-dir = \"{}\"\n\
                 tasklist.default = \"tasks\"\n{extra}",
                dir.join("home").display()
            ),
        )
        .unwrap();
        config
    }

    /// The short id a view prints works with every verb: `task done`
    /// finds a next action of another tasklist than the default one.
    #[test]
    fn a_short_id_from_a_view_works_with_task_done() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(
            dir.path(),
            &[
                ("tasks", task("aaaa1111", "SUMMARY:Here\r\n")),
                ("work", task("bbbb3333", "SUMMARY:There\r\n")),
            ],
            "",
        );

        let next = run(&config, &["gtd", "next"]).unwrap();
        let row = next["tasks"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| row["uid"] == "bbbb3333")
            .unwrap()
            .clone();
        let short = row["shortId"].as_str().unwrap();

        run(&config, &["task", "done", short]).unwrap();
        let body = fs::read_to_string(dir.path().join("home/work/1.ics")).unwrap();
        assert!(body.contains("STATUS:COMPLETED\r\n"), "{body}");

        // NOTE: `-l` narrows the search to that tasklist.
        let err = run(&config, &["task", "undone", "-l", "tasks", short]).unwrap_err();
        assert!(err.to_string().contains("Cannot find task"), "{err}");
    }

    /// A short id is long enough for the whole scope, not only the view:
    /// the Inbox's id of a task sharing a prefix with a next action is
    /// one `gtd clarify` accepts.
    #[test]
    fn a_short_id_tells_apart_every_task_of_the_scope() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(
            dir.path(),
            &[
                (
                    "tasks",
                    task("ffff1", "SUMMARY:Captured\r\nCATEGORIES:inbox\r\n"),
                ),
                ("tasks", task("ffff2", "SUMMARY:Next\r\n")),
            ],
            "",
        );

        let inbox = run(&config, &["gtd", "inbox"]).unwrap();
        let short = inbox["tasks"][0]["shortId"].as_str().unwrap().to_owned();
        assert_eq!(short, "ffff1");

        run(&config, &["gtd", "clarify", &short, "--next"]).unwrap();
        let body = fs::read_to_string(dir.path().join("home/tasks/0.ics")).unwrap();
        assert!(!body.contains("inbox"), "{body}");
    }

    /// A dead account leaves the others listed, named in the output.
    #[test]
    fn an_unreached_account_hides_nothing_else() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(
            dir.path(),
            &[("tasks", task("aaaa1111", "SUMMARY:Here\r\n"))],
            "[accounts.dead]\ncaldav.server = \"http://127.0.0.1:9/dav/\"\n\
             caldav.auth.basic.username = \"x\"\ncaldav.auth.basic.password.raw = \"x\"\n",
        );

        let next = run(&config, &["gtd", "next"]).unwrap();
        assert_eq!(next["tasks"][0]["uid"], "aaaa1111", "{next}");
        assert_eq!(next["unreached"][0]["account"], "dead", "{next}");

        // NOTE: a task not found while an account was unreached says so.
        let err = run(&config, &["gtd", "clarify", "zzzz", "--next"]).unwrap_err();
        assert!(err.to_string().contains("dead"), "{err}");

        // NOTE: alone, the dead account fails.
        assert!(run(&config, &["-a", "dead", "gtd", "next"]).is_err());
    }
}

/// The writer and ordering rules pimdir GUIDE.md §17 decides, run as the
/// binary runs them.
#[cfg(feature = "vdir")]
mod rules {
    use std::{fs, path::Path};

    use clap::Parser;

    use crate::{
        cli::UnfogCli,
        testing::{Kept, task},
    };

    fn run(config: &Path, args: &[&str]) -> serde_json::Value {
        let config = config.to_string_lossy().into_owned();
        let argv = [&["unfog", "-c", config.as_str()], args].concat();
        let cli = UnfogCli::try_parse_from(argv).unwrap();
        let mut printer = Kept::default();
        cli.command
            .unwrap()
            .execute(
                &mut printer,
                &cli.config_paths,
                cli.account.name.as_deref(),
                cli.backend,
            )
            .unwrap();
        printer.outputs.last().cloned().unwrap_or_default()
    }

    fn home(dir: &Path, bodies: &[String]) -> std::path::PathBuf {
        fs::create_dir_all(dir.join("home/tasks")).unwrap();
        for (index, body) in bodies.iter().enumerate() {
            fs::write(dir.join(format!("home/tasks/{index}.ics")), body).unwrap();
        }
        let config = dir.join("config.toml");
        fs::write(
            &config,
            format!(
                "[accounts.personal]\ndefault = true\nvdir.home-dir = \"{}\"\n",
                dir.join("home").display()
            ),
        )
        .unwrap();
        config
    }

    fn body(dir: &Path, index: usize) -> String {
        fs::read_to_string(dir.join(format!("home/tasks/{index}.ics"))).unwrap()
    }

    #[test]
    fn a_plain_category_edit_keeps_the_waiting_state() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(
            dir.path(),
            &[task("w1", "SUMMARY:Quote\r\nCATEGORIES:waiting,work\r\n")],
        );

        run(
            &config,
            &["task", "update", "-l", "tasks", "w1", "--category", "home"],
        );
        let body = body(dir.path(), 0);
        assert!(body.contains("CATEGORIES:waiting,home\r\n"), "{body}");
    }

    #[test]
    fn a_task_already_someday_is_not_rewritten() {
        let dir = tempfile::tempdir().unwrap();
        let original = task("s1", "SUMMARY:Cello\r\nCATEGORIES:Someday\r\n");
        let config = home(dir.path(), std::slice::from_ref(&original));

        run(&config, &["gtd", "clarify", "s1", "--someday"]);
        assert_eq!(body(dir.path(), 0), original);
    }

    #[test]
    fn a_contact_given_an_address_is_written_with_altrep() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(
            dir.path(),
            &[task("i1", "SUMMARY:Lease\r\nCATEGORIES:inbox\r\n")],
        );

        run(
            &config,
            &[
                "gtd",
                "clarify",
                "i1",
                "--waiting",
                "--contact",
                "Jo <jo@example.org>",
            ],
        );
        let body = body(dir.path(), 0);
        assert!(
            body.contains("CONTACT;ALTREP=\"mailto:jo@example.org\":Jo\r\n"),
            "{body}"
        );
    }

    #[test]
    fn a_capture_says_when_it_was_captured() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(dir.path(), &[]);

        let captured = run(&config, &["gtd", "capture", "-l", "tasks", "Buy", "milk"]);
        let uid = captured["uid"].as_str().unwrap();
        let body = fs::read_dir(dir.path().join("home/tasks"))
            .unwrap()
            .map(|entry| fs::read_to_string(entry.unwrap().path()).unwrap())
            .find(|body| body.contains(uid))
            .unwrap();
        assert!(body.contains("\r\nCREATED:"), "{body}");
    }

    /// Next: due first, then priority, then the oldest captured; the
    /// other views the oldest captured first.
    #[test]
    fn the_views_sort_as_the_rules_say() {
        let dir = tempfile::tempdir().unwrap();
        let config = home(
            dir.path(),
            &[
                task("n-late", "SUMMARY:Late\r\nCREATED:20261005T000000Z\r\n"),
                task("n-early", "SUMMARY:Early\r\nCREATED:20261001T000000Z\r\n"),
                task(
                    "n-p1",
                    "SUMMARY:P1\r\nPRIORITY:1\r\nCREATED:20261009T000000Z\r\n",
                ),
                task("n-due", "SUMMARY:Due\r\nDUE;VALUE=DATE:20261101\r\n"),
                task(
                    "i-new",
                    "SUMMARY:New\r\nCATEGORIES:inbox\r\nCREATED:20261020T000000Z\r\n",
                ),
                task(
                    "i-old",
                    "SUMMARY:Old\r\nCATEGORIES:inbox\r\nCREATED:20261002T000000Z\r\n",
                ),
            ],
        );

        let order = |view: &str| -> Vec<String> {
            run(&config, &["gtd", view])["tasks"]
                .as_array()
                .unwrap()
                .iter()
                .map(|row| row["uid"].as_str().unwrap().to_owned())
                .collect()
        };

        assert_eq!(order("next"), ["n-due", "n-p1", "n-early", "n-late"]);
        assert_eq!(order("inbox"), ["i-old", "i-new"]);
    }
}

/// The low-severity rules of the review, run as the binary runs them.
#[cfg(feature = "vdir")]
mod low {
    use std::fs;

    use clap::Parser;

    use crate::{
        cli::UnfogCli,
        testing::{Kept, task},
    };

    fn run(config: &std::path::Path, args: &[&str]) -> anyhow::Result<serde_json::Value> {
        let config = config.to_string_lossy().into_owned();
        let argv = [&["unfog", "-c", config.as_str()], args].concat();
        let cli = UnfogCli::try_parse_from(argv).unwrap();
        let mut printer = Kept::default();
        cli.command.unwrap().execute(
            &mut printer,
            &cli.config_paths,
            cli.account.name.as_deref(),
            cli.backend,
        )?;
        Ok(printer.outputs.last().cloned().unwrap_or_default())
    }

    #[test]
    fn task_list_folds_categories_beyond_ascii() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("home/tasks")).unwrap();
        fs::write(
            dir.path().join("home/tasks/0.ics"),
            task("e1", "SUMMARY:Swim\r\nCATEGORIES:Été\r\n"),
        )
        .unwrap();
        let config = dir.path().join("config.toml");
        fs::write(
            &config,
            format!(
                "[accounts.personal]\ndefault = true\nvdir.home-dir = \"{}\"\n",
                dir.path().join("home").display()
            ),
        )
        .unwrap();

        let listed = run(
            &config,
            &["task", "list", "-l", "tasks", "--category", "ÉTÉ"],
        )
        .unwrap();
        assert_eq!(listed["tasks"][0]["uid"], "e1", "{listed}");
    }

    #[test]
    fn a_quote_in_a_category_filter_is_refused_before_listing() {
        let dir = tempfile::tempdir().unwrap();
        let config = dir.path().join("config.toml");
        fs::write(
            &config,
            "[accounts.dead]\ndefault = true\ncaldav.server = \"http://127.0.0.1:9/dav/\"\n\
             caldav.auth.basic.username = \"x\"\ncaldav.auth.basic.password.raw = \"x\"\n",
        )
        .unwrap();

        let err = run(&config, &["gtd", "next", "-C", "a\"b"]).unwrap_err();
        assert!(err.to_string().contains("double quote"), "{err:#}");
    }
}
