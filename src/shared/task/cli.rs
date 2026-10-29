//! # Task command
//!
//! The `task` command family, the VTODO view over a tasklist, and the
//! time tracked on a task.

use std::path::PathBuf;

use anyhow::Result;
use clap::Subcommand;
use pimalaya_cli::printer::Printer;

use crate::{
    backend::Backend,
    scope::Scope,
    shared::{
        client::TaskClient,
        task::{
            create::TaskCreateCommand,
            delete::TaskDeleteCommand,
            done::{TaskDoneCommand, TaskUndoneCommand},
            list::TaskListCommand,
            read::TaskReadCommand,
            update::TaskUpdateCommand,
        },
    },
    time::track::{StartCommand, StatusCommand, StopCommand},
};

/// Shared API to manage tasks.
///
/// Every subcommand taking a task takes the handle of the item holding
/// it, or a prefix of its UID, dashes ignored, and finds it across every
/// configured account and tasklist, `-a` narrowing to one account and
/// `-l` to one tasklist. `start`, `stop` and `status` span every account
/// too; `list` and `create` work on one tasklist of one account.
#[derive(Debug, Subcommand)]
pub enum TaskCommand {
    #[command(visible_alias = "ls")]
    List(TaskListCommand),
    Read(TaskReadCommand),
    Create(TaskCreateCommand),
    Update(TaskUpdateCommand),
    Done(TaskDoneCommand),
    Undone(TaskUndoneCommand),
    Delete(TaskDeleteCommand),
    Start(StartCommand),
    Stop(StopCommand),
    Status(StatusCommand),
}

impl TaskCommand {
    pub fn execute(
        self,
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
        backend: Backend,
    ) -> Result<()> {
        // NOTE: a listing and a create name their tasklist on one
        // account; every other verb finds its task across the scope.
        let cmd = match self {
            Self::List(cmd) => {
                let client = TaskClient::resolve(printer, config_paths, account_name, backend)?;
                return cmd.execute(printer, client);
            }
            Self::Create(cmd) => {
                let client = TaskClient::resolve(printer, config_paths, account_name, backend)?;
                return cmd.execute(printer, client);
            }
            cmd => cmd,
        };

        let scope = Scope::resolve(printer, config_paths, account_name, backend)?;

        match cmd {
            Self::Read(cmd) => cmd.execute(printer, scope),
            Self::Update(cmd) => cmd.execute(printer, scope),
            Self::Done(cmd) => cmd.execute(printer, scope),
            Self::Undone(cmd) => cmd.execute(printer, scope),
            Self::Delete(cmd) => cmd.execute(printer, scope),
            Self::Start(cmd) => cmd.execute(printer, scope),
            Self::Stop(cmd) => cmd.execute(printer, scope),
            Self::Status(cmd) => cmd.execute(printer, scope),
            Self::List(_) | Self::Create(_) => unreachable!("handled above"),
        }
    }
}
