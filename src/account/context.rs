//! # Account context
//!
//! The merged runtime account every command consumes: the global
//! [`Config`] with the selected `[accounts.<name>]` block folded on top.
//!
//! Defaults belong to the accessor methods rather than to the merge, so
//! an unset field stays distinguishable from a set one and the global
//! block keeps overriding nothing it did not name.

use std::path::PathBuf;

use anyhow::{Result, bail};
use crossterm::style::Color;
use pimalaya_cli::table::{Color as TableColor, ContentArrangement, TableStyle};
use pimalaya_config::command::CommandConfig;

use crate::{
    config::{
        AccountConfig, Config, TableArrangementConfig, TaskListTableConfig,
        TasklistListTableConfig, TimeStrategy,
    },
    shared::table::{DEFAULT_PRESET, style_from_preset},
};

/// A configuration resolved for one run: every option a command reads,
/// each still unset until an accessor supplies its default.
#[derive(Debug, Default)]
pub struct Account {
    /// `comfy_table` preset name every table renders with.
    pub table_preset: Option<String>,
    /// Column-arrangement strategy every table renders with.
    pub table_arrangement: Option<TableArrangementConfig>,
    /// Fallback tasklist id for the `task` commands when their
    /// `-l/--tasklist` flag is omitted.
    pub tasklist_default: Option<String>,
    /// Command a task is edited through, spawned on the path of a
    /// temporary iCalendar file.
    pub task_composer: Option<CommandConfig>,
    /// Per-column colors of the `tasklist list` table.
    pub tasklists_list_table: TasklistListTableConfig,
    /// Per-column colors of the `task list` table.
    pub tasks_list_table: TaskListTableConfig,
    /// Tasklist `capture` writes to.
    pub capture_tasklist: Option<String>,
    /// Where `task start` writes a time entry.
    pub time_strategy: Option<TimeStrategy>,
    /// The calendar the `event` and `journal` strategies write to.
    pub time_collection: Option<String>,
    /// The account holding `time_collection`, when not this one.
    pub time_account: Option<String>,
    /// The directory of the `local` strategy outside pimdir.
    pub time_directory: Option<PathBuf>,
}

impl Account {
    /// Folds `other`'s set fields on top of `self`.
    pub fn merge(self, other: Self) -> Self {
        let tasklist = self.tasklists_list_table;
        let over = other.tasklists_list_table;

        let tasklists_list_table = TasklistListTableConfig {
            id_color: over.id_color.or(tasklist.id_color),
            name_color: over.name_color.or(tasklist.name_color),
            description_color: over.description_color.or(tasklist.description_color),
            color_color: over.color_color.or(tasklist.color_color),
        };

        let task = self.tasks_list_table;
        let over = other.tasks_list_table;

        let tasks_list_table = TaskListTableConfig {
            id_color: over.id_color.or(task.id_color),
            summary_color: over.summary_color.or(task.summary_color),
            due_color: over.due_color.or(task.due_color),
            status_color: over.status_color.or(task.status_color),
            priority_color: over.priority_color.or(task.priority_color),
            categories_color: over.categories_color.or(task.categories_color),
        };

        Self {
            table_preset: other.table_preset.or(self.table_preset),
            table_arrangement: other.table_arrangement.or(self.table_arrangement),
            tasklist_default: other.tasklist_default.or(self.tasklist_default),
            task_composer: other.task_composer.or(self.task_composer),
            tasklists_list_table,
            tasks_list_table,
            capture_tasklist: other.capture_tasklist.or(self.capture_tasklist),
            time_strategy: other.time_strategy.or(self.time_strategy),
            time_collection: other.time_collection.or(self.time_collection),
            time_account: other.time_account.or(self.time_account),
            time_directory: other.time_directory.or(self.time_directory),
        }
    }

    /// The table style the configured preset name maps to.
    pub fn table_style(&self) -> TableStyle {
        style_from_preset(self.table_preset.as_deref().unwrap_or(DEFAULT_PRESET))
    }

    /// The configured column arrangement, dynamic by default.
    pub fn table_arrangement(&self) -> ContentArrangement {
        self.table_arrangement
            .clone()
            .unwrap_or(TableArrangementConfig::Dynamic)
            .into()
    }

    /// Resolves the tasklist id a `task` command operates on: the
    /// `-l/--tasklist` flag wins; otherwise the `tasklist.default`
    /// config is used; otherwise the command bails.
    pub fn tasklist_id(&self, flag: Option<String>) -> Result<String> {
        if let Some(id) = flag.or_else(|| self.tasklist_default.clone()) {
            return Ok(id);
        }

        bail!("Missing tasklist id; pass -l/--tasklist or set tasklist.default")
    }

    /// Resolves the composer a task is edited through.
    ///
    /// The flag wins, taken as a shell line, then the `task.composer`
    /// config, otherwise this bails naming both ways of setting one.
    pub fn task_composer(&self, flag: Option<String>) -> Result<CommandConfig> {
        if let Some(line) = flag {
            return Ok(CommandConfig::Shell(line));
        }

        let Some(composer) = self.task_composer.clone() else {
            bail!(
                "No composer configured; set task.composer or pass --composer <COMMAND>, \
                 which is spawned on the path of the iCalendar to edit"
            )
        };

        Ok(composer)
    }

    /// The strategy `task start` writes a time entry with, `local` by default.
    pub fn time_strategy(&self) -> TimeStrategy {
        self.time_strategy.unwrap_or_default()
    }

    /// The directory the `local` strategy keeps its entries in outside
    /// the pimdir backend: `time.directory`, else
    /// `$XDG_DATA_HOME/unfog/time`, else `~/.local/share/unfog/time`.
    pub fn time_directory(&self) -> Result<PathBuf> {
        if let Some(dir) = &self.time_directory {
            return Ok(dir.clone());
        }

        let data = match std::env::var_os("XDG_DATA_HOME").filter(|dir| !dir.is_empty()) {
            Some(dir) => PathBuf::from(dir),
            None => match std::env::var_os("HOME") {
                Some(home) => PathBuf::from(home).join(".local").join("share"),
                None => bail!("Cannot locate the time directory; set time.directory"),
            },
        };

        Ok(data.join("unfog").join("time"))
    }

    /// Color of the ID column of `tasklist list`, red by default.
    pub fn tasklists_list_table_id_color(&self) -> TableColor {
        map_color_or(self.tasklists_list_table.id_color, Color::Red)
    }
    /// Color of the NAME column of `tasklist list`, green by default.
    pub fn tasklists_list_table_name_color(&self) -> TableColor {
        map_color_or(self.tasklists_list_table.name_color, Color::Green)
    }
    /// Color of the DESCRIPTION column of `tasklist list`, unset by
    /// default.
    pub fn tasklists_list_table_description_color(&self) -> TableColor {
        map_color_or(self.tasklists_list_table.description_color, Color::Reset)
    }
    /// Color of the COLOR column of `tasklist list`, unset by default.
    pub fn tasklists_list_table_color_color(&self) -> TableColor {
        map_color_or(self.tasklists_list_table.color_color, Color::Reset)
    }

    /// Color of the ID column of `task list`, red by default.
    pub fn tasks_list_table_id_color(&self) -> TableColor {
        map_color_or(self.tasks_list_table.id_color, Color::Red)
    }
    /// Color of the SUMMARY column of `task list`, green by default.
    pub fn tasks_list_table_summary_color(&self) -> TableColor {
        map_color_or(self.tasks_list_table.summary_color, Color::Green)
    }
    /// Color of the DUE column of `task list`, yellow by default.
    pub fn tasks_list_table_due_color(&self) -> TableColor {
        map_color_or(self.tasks_list_table.due_color, Color::DarkYellow)
    }
    /// Color of the STATUS column of `task list`, unset by default.
    pub fn tasks_list_table_status_color(&self) -> TableColor {
        map_color_or(self.tasks_list_table.status_color, Color::Reset)
    }
    /// Color of the PRIORITY column of `task list`, unset by default.
    pub fn tasks_list_table_priority_color(&self) -> TableColor {
        map_color_or(self.tasks_list_table.priority_color, Color::Reset)
    }
    /// Color of the CATEGORIES column of `task list`, blue by default.
    pub fn tasks_list_table_categories_color(&self) -> TableColor {
        map_color_or(self.tasks_list_table.categories_color, Color::Blue)
    }
}

/// Maps a [`crossterm::style::Color`] (deserialized from TOML) onto a
/// [`TableColor`], substituting `fallback` when unset.
pub(crate) fn map_color_or(color: Option<Color>, fallback: Color) -> TableColor {
    match color.unwrap_or(fallback) {
        Color::Reset => TableColor::Reset,
        Color::Black => TableColor::Black,
        Color::DarkGrey => TableColor::DarkGrey,
        Color::Red => TableColor::Red,
        Color::DarkRed => TableColor::DarkRed,
        Color::Green => TableColor::Green,
        Color::DarkGreen => TableColor::DarkGreen,
        Color::Yellow => TableColor::Yellow,
        Color::DarkYellow => TableColor::DarkYellow,
        Color::Blue => TableColor::Blue,
        Color::DarkBlue => TableColor::DarkBlue,
        Color::Magenta => TableColor::Magenta,
        Color::DarkMagenta => TableColor::DarkMagenta,
        Color::Cyan => TableColor::Cyan,
        Color::DarkCyan => TableColor::DarkCyan,
        Color::White => TableColor::White,
        Color::Grey => TableColor::Grey,
        Color::Rgb { r, g, b } => TableColor::Rgb { r, g, b },
        Color::AnsiValue(n) => TableColor::AnsiValue(n),
    }
}

impl From<Config> for Account {
    fn from(config: Config) -> Self {
        Self {
            table_preset: config.table.preset,
            table_arrangement: config.table.arrangement,
            tasklist_default: config.tasklist.default,
            task_composer: config.task.composer,
            tasklists_list_table: config.tasklist.list.table,
            tasks_list_table: config.task.list.table,
            capture_tasklist: config.capture.tasklist,
            time_strategy: config.time.strategy,
            time_collection: config.time.collection,
            time_account: config.time.account,
            time_directory: config.time.directory,
        }
    }
}

impl From<AccountConfig> for Account {
    fn from(config: AccountConfig) -> Self {
        Self {
            table_preset: config.table.preset,
            table_arrangement: config.table.arrangement,
            tasklist_default: config.tasklist.default,
            task_composer: config.task.composer,
            tasklists_list_table: config.tasklist.list.table,
            tasks_list_table: config.task.list.table,
            capture_tasklist: config.capture.tasklist,
            time_strategy: config.time.strategy,
            time_collection: config.time.collection,
            time_account: config.time.account,
            time_directory: config.time.directory,
        }
    }
}
