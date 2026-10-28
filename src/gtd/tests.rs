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
        assert_eq!(printer.last()["tasks"][0]["shortId"], "inbo");

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
