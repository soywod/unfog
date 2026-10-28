//! # CalDAV calendar list
//!
//! The `unfog caldav list` command, showing the collections under
//! the resolved home-set with the properties only CalDAV carries.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::{
    printer::Printer,
    table::{Cell, Color, Row, Table, TableStyle},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::caldav::client::CaldavClient;

/// List the CalDAV calendars under the resolved home-set.
///
/// Unlike the shared listing, this shows what only CalDAV carries: the
/// collection change tag, the sync token an incremental sync starts
/// from, and the component kinds the server accepts.
///
/// JSON output: `{"calendars": [{"id", "displayName", "description",
/// "color", "components", "ctag", "syncToken"}]}`.
#[derive(Debug, Parser)]
pub struct CaldavCalendarListCommand;

impl CaldavCalendarListCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: CaldavClient) -> Result<()> {
        let style = client.account.table_style();
        let name_color = client.account.tasklists_list_table_name_color();

        let rows = client
            .list_calendars()?
            .into_iter()
            .map(|calendar| CalendarRow {
                id: calendar.id,
                display_name: calendar.display_name,
                description: calendar.description,
                color: calendar.color,
                components: calendar.components.into_iter().collect(),
                ctag: calendar.ctag,
                sync_token: calendar.sync_token,
            })
            .collect();

        printer.out(CaldavCalendarListOutput {
            style,
            name_color,
            rows,
        })
    }
}

/// The rendered CalDAV calendar listing.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct CaldavCalendarListOutput {
    /// The table style the account renders with.
    #[serde(skip)]
    pub style: TableStyle,
    /// The color the name column is drawn in.
    #[serde(skip)]
    pub name_color: Color,
    /// One row per calendar collection.
    #[serde(rename = "calendars")]
    pub rows: Vec<CalendarRow>,
}

/// One CalDAV calendar with the properties only CalDAV carries.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct CalendarRow {
    /// The last path segment of the collection URL.
    pub id: String,
    /// The DAV display name.
    pub display_name: Option<String>,
    /// The free-form description (RFC 4791 6.2.1).
    pub description: Option<String>,
    /// The display color (RFC 7986 5.9).
    pub color: Option<String>,
    /// The component kinds the collection accepts (RFC 4791 5.2.3).
    ///
    /// Empty means the server declares no restriction.
    pub components: Vec<String>,
    /// The collection change tag, bumped on every change.
    pub ctag: Option<String>,
    /// The RFC 6578 sync token an incremental sync starts from.
    pub sync_token: Option<String>,
}

impl fmt::Display for CaldavCalendarListOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut table = Table::new();

        table
            .load_style(self.style)
            .set_header(Row::from([
                Cell::new("ID"),
                Cell::new("NAME"),
                Cell::new("DESCRIPTION"),
                Cell::new("COLOR"),
                Cell::new("COMPONENTS"),
            ]))
            .add_rows(self.rows.iter().map(|calendar| {
                let mut row = Row::new();
                row.max_height(1)
                    .add_cell(Cell::new(&calendar.id))
                    .add_cell(
                        Cell::new(calendar.display_name.as_deref().unwrap_or(""))
                            .fg(self.name_color),
                    )
                    .add_cell(Cell::new(calendar.description.as_deref().unwrap_or("")))
                    .add_cell(Cell::new(calendar.color.as_deref().unwrap_or("")))
                    .add_cell(Cell::new(calendar.components.join(", ")));
                row
            }));

        writeln!(f)?;
        writeln!(f, "{table}")
    }
}
