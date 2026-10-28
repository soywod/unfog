//! # Tasklist list
//!
//! The `tasklist list` command, printing the account's tasklists as a
//! table.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::{
    printer::Printer,
    table::{Cell, Color, ContentArrangement, Row, Table, TableStyle},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{client::TaskClient, tasklist::Tasklist};

/// List the tasklists of the active account.
///
/// Every backend serves this the same way; use the protocol-specific listings
/// when you need what only one backend exposes.
///
/// JSON output: `{"tasklists": [{"id", "name", "description", "color",
/// "default"}]}`.
#[derive(Debug, Parser)]
pub struct TasklistListCommand {
    /// Maximum width of the rendered table, in terminal columns.
    #[arg(long = "max-width", short = 'w')]
    #[arg(value_name = "COLUMNS")]
    pub max_width: Option<u16>,
}

impl TasklistListCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        let tasklists = client.list_tasklists()?;

        printer.out(TasklistListOutput {
            style: client.account.table_style(),
            arrangement: client.account.table_arrangement(),
            max_width: self.max_width,
            colors: TasklistColors {
                id: client.account.tasklists_list_table_id_color(),
                name: client.account.tasklists_list_table_name_color(),
                description: client.account.tasklists_list_table_description_color(),
                color: client.account.tasklists_list_table_color_color(),
            },
            tasklists,
        })
    }
}

/// The per-column colors a tasklist listing renders with.
#[derive(Clone, Copy, Debug)]
struct TasklistColors {
    id: Color,
    name: Color,
    description: Color,
    color: Color,
}

/// The rendered tasklist listing.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TasklistListOutput {
    #[serde(skip)]
    pub style: TableStyle,
    #[serde(skip)]
    pub arrangement: ContentArrangement,
    #[serde(skip)]
    pub max_width: Option<u16>,
    #[serde(skip)]
    colors: TasklistColors,
    pub tasklists: Vec<Tasklist>,
}

impl fmt::Display for TasklistListOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut table = Table::new();

        table
            .load_style(self.style)
            .set_content_arrangement(self.arrangement.clone())
            .set_header(Row::from(vec![
                Cell::new("ID"),
                Cell::new("NAME"),
                Cell::new("DESCRIPTION"),
                Cell::new("COLOR"),
            ]))
            .add_rows(self.tasklists.iter().map(|tasklist| {
                let mut row = Row::new();
                row.max_height(1);
                row.add_cell(Cell::new(&tasklist.id).fg(self.colors.id));
                row.add_cell(Cell::new(&tasklist.name).fg(self.colors.name));
                row.add_cell(
                    Cell::new(tasklist.description.as_deref().unwrap_or(""))
                        .fg(self.colors.description),
                );
                row.add_cell(
                    Cell::new(tasklist.color.as_deref().unwrap_or("")).fg(self.colors.color),
                );
                row
            }));

        if let Some(width) = self.max_width {
            table.set_width(width);
        }

        writeln!(f)?;
        writeln!(f, "{table}")
    }
}
