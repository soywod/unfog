//! # Vdir collection list
//!
//! The `unfog vdir list` command, showing every collection
//! directory under the configured home directory.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use io_vdir::collection::VdirCollection;
use pimalaya_cli::printer::Printer;
use pimalaya_cli::table::{Cell, Color, Row, Table, TableStyle};
use schemars::JsonSchema;
use serde::Serialize;

use crate::vdir::client::VdirClient;

/// List on-disk vdir collections.
///
/// One row per directory directly under the configured home, with the
/// display name its metadata marker files carry.
///
/// JSON output: `{"collections": [{"id", "displayName", "path"}]}`.
#[derive(Debug, Parser)]
pub struct VdirCollectionListCommand;

impl VdirCollectionListCommand {
    pub fn execute(self, printer: &mut impl Printer, client: VdirClient) -> Result<()> {
        let collections = client.list_collections()?;

        let table = VdirCollectionListOutput {
            style: client.account.table_style(),
            name_color: client.account.tasklists_list_table_name_color(),
            rows: collections.into_iter().map(CollectionRow::from).collect(),
        };

        printer.out(table)
    }
}

/// The rendered vdir collection listing.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct VdirCollectionListOutput {
    /// The table style the account renders with.
    #[serde(skip)]
    pub style: TableStyle,
    /// The color the name column is drawn in.
    #[serde(skip)]
    pub name_color: Color,
    /// One row per collection directory.
    #[serde(rename = "collections")]
    pub rows: Vec<CollectionRow>,
}

impl fmt::Display for VdirCollectionListOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut table = Table::new();

        table
            .load_style(self.style)
            .set_header(Row::from([
                Cell::new("ID"),
                Cell::new("NAME"),
                Cell::new("PATH"),
            ]))
            .add_rows(self.rows.iter().map(|c| {
                let mut row = Row::new();
                row.max_height(1)
                    .add_cell(Cell::new(&c.id))
                    .add_cell(
                        Cell::new(c.display_name.as_deref().unwrap_or("")).fg(self.name_color),
                    )
                    .add_cell(Cell::new(&c.path));
                row
            }));

        writeln!(f)?;
        write!(f, "{table}")?;
        writeln!(f)?;
        Ok(())
    }
}

/// One collection directory, as the listing renders it.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct CollectionRow {
    /// The directory name, which is the collection id.
    pub id: String,
    /// The name the `displayname` marker file carries, if any.
    pub display_name: Option<String>,
    /// The directory's absolute path.
    pub path: String,
}

impl From<VdirCollection> for CollectionRow {
    fn from(collection: VdirCollection) -> Self {
        Self {
            id: collection.id().to_owned(),
            display_name: collection.display_name.clone(),
            path: collection.path.as_str().to_owned(),
        }
    }
}
