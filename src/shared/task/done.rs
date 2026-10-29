//! # Task done and undone
//!
//! The `task done` and `task undone` commands, the partial update a task
//! most often gets.

use core::fmt;

use anyhow::Result;
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::Scope,
    shared::{
        arg::TasklistFilterArg,
        task::fields::{complete, reopen, revise},
    },
};

/// Mark a task as done.
///
/// Sets `STATUS:COMPLETED`, `COMPLETED` to now and `PERCENT-COMPLETE:100`,
/// leaving every other line of the task as it was, and guards the write
/// with the version it read.
///
/// JSON output: `{"id"}`, the task the backend updated.
#[derive(Debug, Parser)]
pub struct TaskDoneCommand {
    #[command(flatten)]
    pub tasklist: TasklistFilterArg,
    /// Task to complete: its handle, or a prefix of its UID.
    #[arg(value_name = "TASK")]
    pub task: String,
}

impl TaskDoneCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Completes the task at `now`.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        let (found, item) = scope.locate(&self.task, self.tasklist.id.as_deref())?;

        let contents = revise(&complete(&item.contents, now)?, now)?;
        scope.client(found.account).update_item(
            &found.tasklist,
            &item.id,
            contents,
            item.etag.as_deref(),
        )?;

        printer.out(TaskDoneOutput {
            done: true,
            id: item.id,
        })
    }
}

/// Mark a task as not done.
///
/// Sets `STATUS:NEEDS-ACTION` and removes `COMPLETED` and
/// `PERCENT-COMPLETE`, leaving every other line of the task as it was.
///
/// JSON output: `{"id"}`, the task the backend updated.
#[derive(Debug, Parser)]
pub struct TaskUndoneCommand {
    #[command(flatten)]
    pub tasklist: TasklistFilterArg,
    /// Task to reopen: its handle, or a prefix of its UID.
    #[arg(value_name = "TASK")]
    pub task: String,
}

impl TaskUndoneCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        let (found, item) = scope.locate(&self.task, self.tasklist.id.as_deref())?;

        let contents = revise(&reopen(&item.contents)?, Timestamp::now())?;
        scope.client(found.account).update_item(
            &found.tasklist,
            &item.id,
            contents,
            item.etag.as_deref(),
        )?;

        printer.out(TaskDoneOutput {
            done: false,
            id: item.id,
        })
    }
}

/// The task `done` or `undone` updated.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskDoneOutput {
    #[serde(skip)]
    done: bool,
    /// Backend-specific identifier of the updated task.
    pub id: String,
}

impl fmt::Display for TaskDoneOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.done {
            true => writeln!(f, "Task `{}` marked as done", self.id),
            false => writeln!(f, "Task `{}` marked as not done", self.id),
        }
    }
}

#[cfg(all(test, feature = "vdir"))]
mod tests {
    use core::fmt;

    use anyhow::Result;
    use clap::Parser;
    use jiff::Timestamp;
    use pimalaya_cli::printer::Printer;
    use serde::Serialize;

    use crate::{
        backend::Backend,
        config::{AccountConfig, Config, VdirConfig},
        shared::{
            client::TaskClient,
            task::done::{TaskDoneCommand, TaskUndoneCommand},
        },
    };

    struct Quiet;

    impl Printer for Quiet {
        fn out<T: fmt::Display + Serialize>(&mut self, _data: T) -> Result<()> {
            Ok(())
        }

        fn is_json(&self) -> bool {
            true
        }
    }

    const TASK: &str = concat!(
        "BEGIN:VCALENDAR\r\n",
        "VERSION:2.0\r\n",
        "PRODID:-//x//y//EN\r\n",
        "BEGIN:VTODO\r\n",
        "UID:9f00aa11-0000-4000-8000-000000000002\r\n",
        "DTSTAMP:20260101T000000Z\r\n",
        "SUMMARY:Call the bank\r\n",
        "STATUS:NEEDS-ACTION\r\n",
        "X-PIMALAYA-GTD:next\r\n",
        "END:VTODO\r\n",
        "END:VCALENDAR\r\n",
    );

    fn client(dir: &tempfile::TempDir) -> TaskClient {
        let account = AccountConfig {
            vdir: Some(VdirConfig {
                home_dir: dir.path().to_path_buf(),
            }),
            ..Default::default()
        };
        TaskClient::new(Config::default(), account, Backend::Auto).unwrap()
    }

    /// The scope of that one account.
    fn scope(dir: &tempfile::TempDir) -> crate::scope::Scope {
        let account = AccountConfig {
            vdir: Some(VdirConfig {
                home_dir: dir.path().to_path_buf(),
            }),
            ..Default::default()
        };
        crate::testing::scope(&crate::testing::config(vec![("personal", account)]))
    }

    #[test]
    fn an_unknown_x_property_survives_done_and_undone() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("tasks")).unwrap();
        let id = client(&dir)
            .create_item("tasks", TASK.as_bytes().to_vec())
            .unwrap();
        let stored = || {
            let item = client(&dir).get_item("tasks", &id).unwrap();
            String::from_utf8(item.contents).unwrap()
        };

        let at: Timestamp = "2026-10-28T08:30:00Z".parse().unwrap();

        TaskDoneCommand::try_parse_from(["done", "-l", "tasks", "9f00"])
            .unwrap()
            .execute_at(&mut Quiet, scope(&dir), at)
            .unwrap();

        assert_eq!(
            stored(),
            TASK.replace("DTSTAMP:20260101T000000Z", "DTSTAMP:20261028T083000Z")
                .replace(
                    "STATUS:NEEDS-ACTION\r\nX-PIMALAYA-GTD:next\r\n",
                    "STATUS:COMPLETED\r\nX-PIMALAYA-GTD:next\r\n\
                     COMPLETED:20261028T083000Z\r\nPERCENT-COMPLETE:100\r\n"
                )
        );

        TaskUndoneCommand::try_parse_from(["undone", "-l", "tasks", &id])
            .unwrap()
            .execute(&mut Quiet, scope(&dir))
            .unwrap();

        let reopened = stored();
        assert!(reopened.contains("STATUS:NEEDS-ACTION\r\n"), "{reopened}");
        assert!(reopened.contains("X-PIMALAYA-GTD:next\r\n"), "{reopened}");
        assert!(!reopened.contains("COMPLETED:"), "{reopened}");
        assert!(!reopened.contains("PERCENT-COMPLETE"), "{reopened}");
    }
}
