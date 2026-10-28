//! # Vdir CLI
//!
//! The `vdir` command family: the filesystem-specific verbs, each
//! handed a [`VdirClient`] rooted at the configured home directory.

use anyhow::Result;
use clap::Subcommand;
use pimalaya_cli::printer::Printer;

use crate::vdir::{
    client::VdirClient, create::VdirCollectionCreateCommand, delete::VdirCollectionDeleteCommand,
    list::VdirCollectionListCommand, rename::VdirCollectionRenameCommand,
};

/// Vdir CLI.
///
/// Direct access to the on-disk vdir backend: list, create, rename,
/// delete collections without going through the shared calendar API.
#[derive(Debug, Subcommand)]
#[command(rename_all = "kebab-case")]
pub enum VdirCommand {
    #[command(visible_alias = "ls")]
    List(VdirCollectionListCommand),
    Create(VdirCollectionCreateCommand),
    Rename(VdirCollectionRenameCommand),
    Delete(VdirCollectionDeleteCommand),
}

impl VdirCommand {
    pub fn execute(self, printer: &mut impl Printer, client: VdirClient) -> Result<()> {
        match self {
            Self::List(cmd) => cmd.execute(printer, client),
            Self::Create(cmd) => cmd.execute(printer, client),
            Self::Rename(cmd) => cmd.execute(printer, client),
            Self::Delete(cmd) => cmd.execute(printer, client),
        }
    }
}
