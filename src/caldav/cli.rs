//! # CalDAV CLI
//!
//! The `caldav` command family: the protocol-specific verbs, each
//! handed a connected [`CaldavClient`].

use anyhow::Result;
use clap::Subcommand;
use pimalaya_cli::printer::Printer;

use crate::caldav::{
    client::CaldavClient, create::CaldavCalendarCreateCommand, delete::CaldavCalendarDeleteCommand,
    discover::CaldavDiscoverCommand, list::CaldavCalendarListCommand,
};

/// CalDAV CLI.
///
/// Direct access to the CalDAV backend: discover endpoints, list,
/// create, delete calendars without going through the shared API.
#[derive(Debug, Subcommand)]
#[command(rename_all = "kebab-case")]
pub enum CaldavCommand {
    Discover(CaldavDiscoverCommand),
    #[command(visible_alias = "ls")]
    List(CaldavCalendarListCommand),
    Create(CaldavCalendarCreateCommand),
    Delete(CaldavCalendarDeleteCommand),
}

impl CaldavCommand {
    pub fn execute(self, printer: &mut impl Printer, client: CaldavClient) -> Result<()> {
        match self {
            Self::Discover(cmd) => cmd.execute(printer, client),
            Self::List(cmd) => cmd.execute(printer, client),
            Self::Create(cmd) => cmd.execute(printer, client),
            Self::Delete(cmd) => cmd.execute(printer, client),
        }
    }
}
