//! # CalDAV calendar delete
//!
//! The `unfog caldav delete` command, removing one calendar
//! collection from the server.

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::printer::{Message, Printer};

use crate::caldav::client::CaldavClient;

/// Delete a CalDAV calendar by id.
///
/// The DELETE takes the collection and every resource filed in it.
///
/// JSON output: `{"message": "..."}`.
#[derive(Debug, Parser)]
pub struct CaldavCalendarDeleteCommand {
    /// Tasklist identifier (last path segment of the calendar URL).
    #[arg(value_name = "ID")]
    pub id: String,
}

impl CaldavCalendarDeleteCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: CaldavClient) -> Result<()> {
        client.delete_calendar(&self.id)?;

        let msg = format!("Tasklist `{}` successfully deleted", self.id);
        printer.out(Message::new(msg))
    }
}
