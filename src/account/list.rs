//! # Account list
//!
//! The `account list` command, printing every account the configuration
//! declares as a table.

use std::{fmt, path::PathBuf};

use anyhow::Result;
use clap::Parser;
use crossterm::style::Color as CrosstermColor;
use pimalaya_cli::{
    printer::Printer,
    table::{Cell, Color, ContentArrangement, Row, Table, TableStyle},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    account::context::map_color_or,
    cli::load_config,
    config::{AccountConfig, TableArrangementConfig},
    shared::table::{DEFAULT_PRESET, style_from_preset},
};

/// List the accounts declared in the configuration.
///
/// Each row shows the account name, the backends it carries a
/// configuration block for, and whether it is the default account.
///
/// JSON output: `{"accounts": [{"name", "default", "backends"}]}`.
#[derive(Debug, Parser)]
pub struct AccountListCommand;

impl AccountListCommand {
    pub fn execute(self, printer: &mut impl Printer, config_paths: &[PathBuf]) -> Result<()> {
        let config = load_config(printer, config_paths)?;

        let style = style_from_preset(config.table.preset.as_deref().unwrap_or(DEFAULT_PRESET));
        let arrangement = config
            .table
            .arrangement
            .clone()
            .unwrap_or(TableArrangementConfig::Dynamic)
            .into();

        let table_config = &config.account.list.table;
        let colors = AccountColors {
            name: map_color_or(table_config.name_color, CrosstermColor::Green),
            backends: map_color_or(table_config.backends_color, CrosstermColor::Blue),
            default: map_color_or(table_config.default_color, CrosstermColor::Reset),
        };

        let mut accounts: Vec<AccountRow> = config
            .accounts
            .iter()
            .map(|(name, account)| AccountRow::from_account(name, account))
            .collect();
        accounts.sort_by(|a, b| a.name.cmp(&b.name));

        printer.out(AccountListOutput {
            style,
            arrangement,
            colors,
            accounts,
        })
    }
}

/// The per-column colors an account listing renders with.
#[derive(Clone, Copy, Debug)]
struct AccountColors {
    /// Color of the NAME column.
    name: Color,
    /// Color of the BACKENDS column.
    backends: Color,
    /// Color of the DEFAULT column.
    default: Color,
}

/// One account's row in an [`AccountListOutput`].
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct AccountRow {
    /// The `[accounts.<name>]` table key.
    pub name: String,
    /// Whether the account is flagged `default = true`.
    pub default: bool,
    /// The backends the account carries a configuration block for.
    pub backends: Vec<&'static str>,
}

impl AccountRow {
    fn from_account(name: &str, account: &AccountConfig) -> Self {
        let mut backends = Vec::new();

        #[cfg(feature = "vdir")]
        if account.vdir.is_some() {
            backends.push("vdir");
        }

        #[cfg(feature = "pimdir")]
        if account.pimdir.is_some() {
            backends.push("pimdir");
        }

        #[cfg(feature = "caldav")]
        if account.caldav.is_some() {
            backends.push("caldav");
        }

        Self {
            name: name.to_owned(),
            default: account.default,
            backends,
        }
    }
}

/// The rendered account listing.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct AccountListOutput {
    /// The `comfy_table` style the configured preset maps to.
    #[serde(skip)]
    pub style: TableStyle,
    /// How the columns spread over the terminal width.
    #[serde(skip)]
    pub arrangement: ContentArrangement,
    /// The per-column colors.
    #[serde(skip)]
    colors: AccountColors,
    /// One row per account, sorted by name.
    pub accounts: Vec<AccountRow>,
}

impl fmt::Display for AccountListOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut table = Table::new();

        table
            .load_style(self.style)
            .set_content_arrangement(self.arrangement.clone())
            .set_header(Row::from(vec![
                Cell::new("NAME"),
                Cell::new("BACKENDS"),
                Cell::new("DEFAULT"),
            ]))
            .add_rows(self.accounts.iter().map(|account| {
                let mut row = Row::new();
                row.max_height(1);
                row.add_cell(Cell::new(&account.name).fg(self.colors.name));
                row.add_cell(Cell::new(account.backends.join(", ")).fg(self.colors.backends));
                row.add_cell(
                    Cell::new(if account.default { "yes" } else { "" }).fg(self.colors.default),
                );
                row
            }));

        writeln!(f)?;
        writeln!(f, "{table}")
    }
}
