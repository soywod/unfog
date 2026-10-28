//! # Tasklist create
//!
//! The `tasklist create` command, adding a tasklist to the account.

use core::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::client::TaskClient;

/// Create a new tasklist.
///
/// The reported id is the one the backend assigned, and it is what later
/// commands address the tasklist by. A CalDAV tasklist is a calendar
/// created to hold VTODOs alone.
///
/// JSON output: `{"id"}`, the identifier the backend assigned.
#[derive(Debug, Parser)]
pub struct TasklistCreateCommand {
    /// Stable tasklist identifier.
    #[arg(value_name = "ID")]
    pub id: String,
    /// Human-readable display name.
    #[arg(value_name = "NAME")]
    pub name: String,
    /// Free-form description.
    #[arg(short, long, value_name = "TEXT")]
    pub description: Option<String>,
    /// Hex color (`#RRGGBB`).
    #[arg(long, value_name = "HEX")]
    pub color: Option<String>,
}

impl TasklistCreateCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        let id = client.create_tasklist(
            &self.id,
            &self.name,
            self.description.as_deref(),
            self.color.as_deref(),
        )?;

        printer.out(TasklistCreateOutput { id })
    }
}

/// The tasklist the backend created.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TasklistCreateOutput {
    /// Backend-assigned identifier of the new tasklist.
    pub id: String,
}

impl fmt::Display for TasklistCreateOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Tasklist `{}` successfully created", self.id)
    }
}
