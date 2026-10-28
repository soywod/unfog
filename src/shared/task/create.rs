//! # Task create
//!
//! The `task create` command, storing one VTODO into the selected
//! tasklist, from an iCalendar source, from field flags, from the
//! composer, or from any combination of the three.

use core::fmt;

use anyhow::{Result, bail};
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{
    arg::{IcalComposerArgs, TasklistIdArg},
    client::TaskClient,
    composer::IcalComposer,
    ical::{blank_task, ensure_valid, read_source},
    task::fields::{TaskFieldsArgs, revise},
};

/// Create a new task.
///
/// The source, the field flags and `-i` stack: the source is the task to
/// start from, the flags set the properties they name on it, and `-i`
/// opens the result in the composer. With no source the task is minted
/// fresh, carrying nothing but a `UID` and a `DTSTAMP`:
/// `unfog task create --summary "Buy milk" --due 2026-10-30`.
///
/// A task built from flags alone is checked before it is written; a
/// source goes to the backend as written, its `DTSTAMP` set to the time
/// of the write.
///
/// JSON output: `{"id"}`, the identifier the backend assigned.
#[derive(Debug, Parser)]
pub struct TaskCreateCommand {
    #[command(flatten)]
    pub tasklist: TasklistIdArg,
    /// The composer the task is refined in before it is written.
    #[command(flatten)]
    pub composer: IcalComposerArgs,
    /// The properties the command sets on the task.
    #[command(flatten)]
    pub fields: TaskFieldsArgs,
    /// iCalendar to store: a path to a file, raw iCalendar contents, or
    /// `-` for stdin. Omit to mint one.
    #[arg(value_name = "ICAL")]
    pub ical: Option<String>,
}

impl TaskCreateCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        if self.ical.is_none() && self.fields.is_empty() && !self.composer.interactive {
            bail!("Nothing to create; give an iCalendar, a field flag, or -i to compose one");
        }

        let tasklist_id = client.account.tasklist_id(self.tasklist.id)?;

        let base = match &self.ical {
            Some(source) => read_source(source)?,
            None => blank_task()?,
        };

        let seed = self.fields.apply(&base)?;

        if !self.composer.interactive {
            // NOTE: only a task unfog minted is checked. An iCalendar
            // given on the command line goes to the backend as it was
            // written.
            if self.ical.is_none() {
                ensure_valid(&seed)?;
            }

            let contents = revise(&seed, Timestamp::now())?;
            let id = client.create_item(&tasklist_id, contents)?;
            return printer.out(TaskCreateOutput::Created(TaskCreatedOutput { id }));
        }

        let composer = IcalComposer {
            command: client.account.task_composer(self.composer.composer)?,
        };

        // NOTE: nothing has reached the network yet, the client opening
        // on the first call that needs it, so the editor runs with no
        // connection held and the create below opens one.
        let Some(draft) = composer.edit(printer, &seed)? else {
            return printer.out(TaskCreateOutput::Abandoned {});
        };

        let created = revise(&draft.contents, Timestamp::now())
            .and_then(|contents| client.create_item(&tasklist_id, contents));
        let id = draft.finish(created)?;

        printer.out(TaskCreateOutput::Created(TaskCreatedOutput { id }))
    }
}

/// What `task create` prints, which is whether it wrote anything.
///
/// Untagged, so the write serializes exactly as it would on its own. The
/// second shape is reachable through `-i` alone, which `--json` refuses
/// to run.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(untagged)]
pub enum TaskCreateOutput {
    /// The task the backend created.
    Created(TaskCreatedOutput),
    /// Nothing, the edit having been abandoned: an empty object rather
    /// than null, so every payload of the command is a JSON object.
    Abandoned {},
}

impl fmt::Display for TaskCreateOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Created(out) => out.fmt(f),
            Self::Abandoned {} => writeln!(f, "Task not created"),
        }
    }
}

/// The task the backend created.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskCreatedOutput {
    /// Backend-assigned identifier of the new task.
    pub id: String,
}

impl fmt::Display for TaskCreatedOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Task `{}` successfully created", self.id)
    }
}
