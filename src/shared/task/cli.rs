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
/// it, or a prefix of its UID, dashes ignored. `start`, `stop` and
/// `status` span every configured account, `-a` narrowing them to one;
/// the others work on one account.
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
        // NOTE: time tracking spans every account, so it resolves a
        // scope rather than one account's client.
        let cmd = match self {
            Self::Start(cmd) => {
                let scope = Scope::resolve(printer, config_paths, account_name, backend)?;
                return cmd.execute(printer, scope);
            }
            Self::Stop(cmd) => {
                let scope = Scope::resolve(printer, config_paths, account_name, backend)?;
                return cmd.execute(printer, scope);
            }
            Self::Status(cmd) => {
                let scope = Scope::resolve(printer, config_paths, account_name, backend)?;
                return cmd.execute(printer, scope);
            }
            cmd => cmd,
        };

        let client = TaskClient::resolve(printer, config_paths, account_name, backend)?;

        match cmd {
            Self::List(cmd) => cmd.execute(printer, client),
            Self::Read(cmd) => cmd.execute(printer, client),
            Self::Create(cmd) => cmd.execute(printer, client),
            Self::Update(cmd) => cmd.execute(printer, client),
            Self::Done(cmd) => cmd.execute(printer, client),
            Self::Undone(cmd) => cmd.execute(printer, client),
            Self::Delete(cmd) => cmd.execute(printer, client),
            Self::Start(_) | Self::Stop(_) | Self::Status(_) => unreachable!("handled above"),
        }
    }
}
