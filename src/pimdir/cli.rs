//! # Pimdir CLI
//!
//! The `pimdir` command family, reporting on the local store the
//! account reads from.

use anyhow::Result;
use clap::Subcommand;
use pimalaya_cli::printer::Printer;

use crate::pimdir::{backend::PimdirBackend, status::PimdirStatusCommand};

/// pimdir CLI.
///
/// Direct access to the local pimdir store behind the account: what it
/// holds and how much of it is downloaded.
///
/// The store's own operator tooling, the `pimdir` binary io-pimdir
/// ships, covers the rest, including the queue and the retained items.
#[derive(Debug, Subcommand)]
#[command(rename_all = "kebab-case")]
pub enum PimdirCommand {
    Status(PimdirStatusCommand),
}

impl PimdirCommand {
    pub fn execute(self, printer: &mut impl Printer, backend: PimdirBackend) -> Result<()> {
        match self {
            Self::Status(cmd) => cmd.execute(printer, backend),
        }
    }
}
