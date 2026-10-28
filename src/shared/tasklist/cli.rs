//! # Tasklist command
//!
//! The `tasklist` command family, the portable view over the collections
//! an account holds tasks in.

use anyhow::Result;
use clap::Subcommand;
use pimalaya_cli::printer::Printer;

use crate::shared::{
    client::TaskClient,
    tasklist::{
        create::TasklistCreateCommand, delete::TasklistDeleteCommand, list::TasklistListCommand,
    },
};

/// Shared API to manage the tasklists of the active account.
#[derive(Debug, Subcommand)]
pub enum TasklistCommand {
    #[command(visible_alias = "ls")]
    List(TasklistListCommand),
    Create(TasklistCreateCommand),
    Delete(TasklistDeleteCommand),
}

impl TasklistCommand {
    pub fn execute(self, printer: &mut impl Printer, client: TaskClient) -> Result<()> {
        match self {
            Self::List(cmd) => cmd.execute(printer, client),
            Self::Create(cmd) => cmd.execute(printer, client),
            Self::Delete(cmd) => cmd.execute(printer, client),
        }
    }
}
