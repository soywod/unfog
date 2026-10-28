//! # CalDAV calendar create
//!
//! The `unfog caldav create` command, issuing MKCALENDAR under the
//! resolved calendar home-set.

use anyhow::Result;
use clap::Parser;
use io_webdav::rfc4791::calendar::CaldavCalendar;
use pimalaya_cli::printer::{Message, Printer};

use crate::caldav::client::CaldavClient;

/// Create a CalDAV calendar under the resolved calendar home-set.
///
/// JSON output: `{"message": "..."}`.
#[derive(Debug, Parser)]
pub struct CaldavCalendarCreateCommand {
    /// Tasklist identifier (last path segment of the calendar URL).
    #[arg(value_name = "ID")]
    pub id: String,

    /// Human-readable display name.
    #[arg(short, long, value_name = "NAME")]
    pub display_name: Option<String>,

    /// Free-form description.
    #[arg(short = 'D', long, value_name = "TEXT")]
    pub description: Option<String>,

    /// Hex color (`#RRGGBB`).
    #[arg(long, value_name = "HEX")]
    pub color: Option<String>,

    /// Component kinds the calendar accepts (RFC 4791 5.2.3), such as
    /// VEVENT or VTODO. Repeat the flag for several.
    ///
    /// A server fixes this at creation and refuses to change it
    /// afterwards.
    #[arg(long = "component", value_name = "KIND")]
    pub components: Vec<String>,

    /// Default time zone, as a whole VTIMEZONE block.
    #[arg(long, value_name = "VTIMEZONE")]
    pub tz: Option<String>,
}

impl CaldavCalendarCreateCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: CaldavClient) -> Result<()> {
        let calendar = CaldavCalendar {
            id: self.id.clone(),
            display_name: self.display_name,
            description: self.description,
            color: self.color,
            components: self.components.into_iter().collect(),
            tz: self.tz,
            ..Default::default()
        };

        client.create_calendar(&calendar)?;

        let msg = format!("Tasklist `{}` successfully created", self.id);
        printer.out(Message::new(msg))
    }
}
