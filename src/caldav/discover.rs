//! # CalDAV discover
//!
//! The `unfog caldav discover` command, walking the RFC 6764
//! discovery chain and printing every URL it resolves.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::caldav::client::CaldavClient;

/// Run the CalDAV discovery chain.
///
/// Walks the RFC 6764 `.well-known/caldav` path, then the
/// current-user-principal, then the calendar home-set, printing each
/// URL it resolves.
///
/// JSON output: `{"principal", "calendarHomeSet"}`.
#[derive(Debug, Parser)]
pub struct CaldavDiscoverCommand;

impl CaldavDiscoverCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: CaldavClient) -> Result<()> {
        let principal = client.current_user_principal()?;
        let home = client.calendar_home_set()?;

        printer.out(CaldavDiscoverOutput {
            principal: principal.to_string(),
            calendar_home_set: home.to_string(),
        })
    }
}

/// The URLs the discovery chain resolved.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct CaldavDiscoverOutput {
    /// The principal the server reports for these credentials.
    pub principal: String,
    /// The collection the principal files its calendars under.
    pub calendar_home_set: String,
}

impl fmt::Display for CaldavDiscoverOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Principal: {}", self.principal)?;
        writeln!(f, "Tasklist home-set: {}", self.calendar_home_set)?;
        Ok(())
    }
}
