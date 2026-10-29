//! # Task update
//!
//! The `task update` command, replacing one VTODO of the selected
//! tasklist, from an iCalendar source, from field flags, from the
//! composer, or from any combination of the three.

use core::fmt;

use anyhow::{Result, bail};
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::Scope,
    shared::{
        arg::{IcalComposerArgs, TasklistFilterArg},
        composer::IcalComposer,
        ical::read_source,
        task::fields::{TaskFieldsArgs, revise},
    },
};

/// Change an existing task.
///
/// The source, the field flags and `-i` stack: the source replaces the
/// task's bytes, the flags set the properties they name on it, and `-i`
/// opens the result in the composer. A source replaces the whole
/// component, so it has to carry every property the task keeps.
///
/// With no source the task is read from the backend first, and the
/// version it answered guards the write: `--summary "New title"` alone
/// changes the title and leaves every other line as it was. An empty
/// text flag removes its property: `--due ""`.
///
/// Whatever it writes, the task is marked as revised: `DTSTAMP` is set to
/// the time of the write, in UTC, and `LAST-MODIFIED` to the same instant
/// when the task carries one.
///
/// JSON output: `{"id"}`, the task the backend updated.
#[derive(Debug, Parser)]
pub struct TaskUpdateCommand {
    #[command(flatten)]
    pub tasklist: TasklistFilterArg,
    /// The composer the task is refined in before it is written.
    #[command(flatten)]
    pub composer: IcalComposerArgs,
    /// The properties the command sets on the task.
    #[command(flatten)]
    pub fields: TaskFieldsArgs,
    /// Task to update: its handle, or a prefix of its UID.
    #[arg(value_name = "TASK")]
    pub task: String,
    /// iCalendar replacing the current contents: a path to a file, raw
    /// iCalendar contents, or `-` for stdin. Omit to start from the
    /// stored task.
    #[arg(value_name = "ICAL")]
    pub ical: Option<String>,
}

impl TaskUpdateCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now)
    }

    /// Runs the update, `now` telling the instant the task is revised at
    /// when it is written.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: impl Fn() -> Timestamp,
    ) -> Result<()> {
        if self.ical.is_none() && self.fields.is_empty() && !self.composer.interactive {
            bail!("Nothing to update; give an iCalendar, a field flag, or -i to edit the task");
        }

        // NOTE: finding the stored task also gives the version it is
        // being changed from, so an edit that took a minute cannot
        // silently overwrite a write that landed during it.
        let (found, item) = scope.locate(&self.task, self.tasklist.id.as_deref())?;
        let tasklist_id = found.tasklist;
        let client = scope.client(found.account);

        let base = match &self.ical {
            Some(source) => read_source(source)?,
            None => item.contents,
        };

        let if_match = item.etag;
        let seed = self.fields.apply(&base)?;

        if !self.composer.interactive {
            let contents = revise(&seed, now())?;
            client.update_item(&tasklist_id, &item.id, contents, if_match.as_deref())?;

            return printer.out(TaskUpdateOutput::Applied(TaskUpdatedOutput { id: item.id }));
        }

        let composer = IcalComposer {
            command: client.account.task_composer(self.composer.composer)?,
        };

        // NOTE: reading the task is the one thing that has to happen
        // before the editor, so its connection is dropped here rather
        // than left idle for the minutes an edit takes.
        client.disconnect();

        let Some(draft) = composer.edit(printer, &seed)? else {
            return printer.out(TaskUpdateOutput::Abandoned {});
        };

        let updated = revise(&draft.contents, now()).and_then(|contents| {
            client.update_item(&tasklist_id, &item.id, contents, if_match.as_deref())
        });

        draft.finish(updated)?;

        printer.out(TaskUpdateOutput::Applied(TaskUpdatedOutput { id: item.id }))
    }
}

/// What `task update` prints, which is whether it wrote anything.
///
/// Untagged, so the write serializes exactly as it would on its own. The
/// second shape is reachable through `-i` alone, which `--json` refuses
/// to run.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(untagged)]
pub enum TaskUpdateOutput {
    /// The task the backend updated.
    Applied(TaskUpdatedOutput),
    /// Nothing, the edit having been abandoned: an empty object rather
    /// than null, so every payload of the command is a JSON object.
    Abandoned {},
}

impl fmt::Display for TaskUpdateOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Applied(out) => out.fmt(f),
            Self::Abandoned {} => writeln!(f, "Task not updated"),
        }
    }
}

/// The task the backend updated.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskUpdatedOutput {
    /// Backend-specific identifier of the updated task.
    pub id: String,
}

impl fmt::Display for TaskUpdatedOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Task `{}` successfully updated", self.id)
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
        shared::{client::TaskClient, task::update::TaskUpdateCommand},
    };

    /// A printer dropping what it is handed.
    struct Quiet;

    impl Printer for Quiet {
        fn out<T: fmt::Display + Serialize>(&mut self, _data: T) -> Result<()> {
            Ok(())
        }

        fn is_json(&self) -> bool {
            true
        }
    }

    /// A task another client wrote: folded, with a parameterized X-
    /// property, an alarm and a property order of its own.
    const TASK: &str = concat!(
        "BEGIN:VCALENDAR\r\n",
        "PRODID:-//Other Client//EN\r\n",
        "VERSION:2.0\r\n",
        "BEGIN:VTODO\r\n",
        "SUMMARY:Write the report\r\n",
        "UID:4c1e0b2a-7d3e-4f00-9b1a-000000000001\r\n",
        "DTSTAMP:20260101T000000Z\r\n",
        "DESCRIPTION:A description long enough to be folded by the client that \r\n",
        " wrote it\r\n",
        "X-OTHER-CLIENT;X-FLAG=1:kept as is\r\n",
        "BEGIN:VALARM\r\n",
        "ACTION:AUDIO\r\n",
        "TRIGGER:-PT5M\r\n",
        "END:VALARM\r\n",
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

    /// A vdir home holding one tasklist, `tasks`, carrying `body`.
    fn home(body: &str) -> (tempfile::TempDir, String) {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("tasks")).unwrap();
        let id = client(&dir)
            .create_item("tasks", body.as_bytes().to_vec())
            .unwrap();
        (dir, id)
    }

    fn stored(dir: &tempfile::TempDir, id: &str) -> String {
        let item = client(dir).get_item("tasks", id).unwrap();
        String::from_utf8(item.contents).unwrap()
    }

    fn at() -> Timestamp {
        "2026-10-28T08:30:00Z".parse().unwrap()
    }

    #[test]
    fn a_flag_edit_of_a_foreign_task_changes_its_line_and_the_stamp_alone() {
        let (dir, id) = home(TASK);

        TaskUpdateCommand::try_parse_from(["update", "-l", "tasks", "4c1e", "--summary", "New"])
            .unwrap()
            .execute_at(&mut Quiet, scope(&dir), at)
            .unwrap();

        assert_eq!(
            stored(&dir, &id),
            TASK.replace("SUMMARY:Write the report", "SUMMARY:New")
                .replace("DTSTAMP:20260101T000000Z", "DTSTAMP:20261028T083000Z")
        );
    }

    #[test]
    fn nothing_to_update_writes_nothing() {
        let (dir, id) = home(TASK);

        let err = TaskUpdateCommand::try_parse_from(["update", "-l", "tasks", "4c1e"])
            .unwrap()
            .execute_at(&mut Quiet, scope(&dir), at)
            .unwrap_err();

        assert!(err.to_string().contains("Nothing to update"), "{err}");
        assert_eq!(stored(&dir, &id), TASK);
    }
}
