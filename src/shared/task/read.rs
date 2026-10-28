//! # Task read
//!
//! The `task read` command, printing one task's fields or the exact
//! bytes the backend stores.

use std::fmt;

use anyhow::{Result, bail};
use clap::Parser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{
    arg::TasklistIdArg,
    client::TaskClient,
    task::{Task, find::find_task},
};

/// Read a single task.
///
/// Prints the task's fields, or with `--raw` the iCalendar exactly as
/// the backend stores it.
///
/// JSON output: `{"task": {...}, "contents"}`, the projection and the raw
/// iCalendar as text.
#[derive(Debug, Parser)]
pub struct TaskReadCommand {
    #[command(flatten)]
    pub tasklist: TasklistIdArg,
    /// Print the stored iCalendar bytes instead of the fields.
    #[arg(long)]
    pub raw: bool,
    /// Task to read: its handle, or a prefix of its UID.
    #[arg(value_name = "TASK")]
    pub task: String,
}

impl TaskReadCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        let tasklist_id = client.account.tasklist_id(self.tasklist.id)?;
        let item = find_task(&mut client, &tasklist_id, &self.task)?;

        let Some(task) = Task::project(&item).into_iter().next() else {
            bail!("Item `{}` holds no VTODO", item.id);
        };

        let contents = String::from_utf8_lossy(&item.contents).into_owned();

        printer.out(TaskReadOutput {
            raw: self.raw,
            task,
            contents,
        })
    }
}

/// The read task: its projection and its stored bytes.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskReadOutput {
    #[serde(skip)]
    raw: bool,
    /// The task's fields.
    pub task: Task,
    /// The raw iCalendar bytes, lossily decoded as UTF-8.
    pub contents: String,
}

impl fmt::Display for TaskReadOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.raw {
            return writeln!(f, "{}", self.contents.trim_end());
        }

        let task = &self.task;
        let mut line = |label: &str, value: Option<&str>| match value {
            Some(value) if !value.is_empty() => writeln!(f, "{label:<12}{value}"),
            _ => Ok(()),
        };

        line("Id", Some(&task.id))?;
        line("UID", Some(&task.uid))?;
        line("Summary", Some(&task.summary))?;
        line("Status", task.status.as_deref())?;
        line("Progress", Some(&task.progress()))?;
        line("Start", task.start.as_deref())?;
        line("Due", task.due.as_deref())?;
        line("Completed", task.completed.as_deref())?;
        line("Priority", task.priority.map(|p| p.to_string()).as_deref())?;
        line("Categories", Some(&task.categories.join(", ")))?;
        line("Parent", task.parent.as_deref())?;
        line("URL", task.url.as_deref())?;
        line("Alarms", Some(&task.alarms.join(", ")))?;

        if let Some(description) = &task.description {
            writeln!(f)?;
            writeln!(f, "{}", description.trim_end())?;
        }

        Ok(())
    }
}
