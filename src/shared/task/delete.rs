//! # Task delete
//!
//! The `task delete` command, removing the item holding one task.

use core::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{arg::TasklistIdArg, client::TaskClient, task::find::find_task};

/// Delete a single task.
///
/// The item holding the task is removed, with no confirmation prompt
/// and no copy kept, gated on the version it was found at.
///
/// JSON output: `{"id"}`, the item deleted.
#[derive(Debug, Parser)]
pub struct TaskDeleteCommand {
    #[command(flatten)]
    pub tasklist: TasklistIdArg,
    /// Task to delete: its handle, or a prefix of its UID.
    #[arg(value_name = "TASK")]
    pub task: String,
}

impl TaskDeleteCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        let tasklist_id = client.account.tasklist_id(self.tasklist.id)?;
        let item = find_task(&mut client, &tasklist_id, &self.task)?;

        client.delete_item(&tasklist_id, &item.id, item.etag.as_deref())?;

        printer.out(TaskDeleteOutput { id: item.id })
    }
}

/// The task the backend deleted.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskDeleteOutput {
    /// Handle of the deleted item.
    pub id: String,
}

impl fmt::Display for TaskDeleteOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Task `{}` successfully deleted", self.id)
    }
}
