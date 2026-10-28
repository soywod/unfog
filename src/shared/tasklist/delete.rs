//! # Tasklist delete
//!
//! The `tasklist delete` command, removing a tasklist and everything in
//! it.

use core::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::client::TaskClient;

/// Delete the given tasklist and every task it contains.
///
/// JSON output: `{"id"}`, the tasklist deleted.
#[derive(Debug, Parser)]
pub struct TasklistDeleteCommand {
    /// Tasklist to delete.
    ///
    /// Mandatory: unlike the other commands, deletion never falls back
    /// to the `tasklist.default` config.
    #[arg(value_name = "ID")]
    pub id: String,
}

impl TasklistDeleteCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        client.delete_tasklist(&self.id)?;
        printer.out(TasklistDeleteOutput { id: self.id })
    }
}

/// The tasklist the backend deleted.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TasklistDeleteOutput {
    /// Identifier of the deleted tasklist.
    pub id: String,
}

impl fmt::Display for TasklistDeleteOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Tasklist `{}` successfully deleted", self.id)
    }
}
