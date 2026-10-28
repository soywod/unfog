//! # Views
//!
//! `inbox`, `next`, `waiting` and `someday`: the open tasks of every
//! account and tasklist of the scope, sorted as `task list` sorts them.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::{
    printer::Printer,
    table::{Cell, Color, ContentArrangement, Row, Table, TableStyle},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::{Located, Scope},
    shared::task::{
        Task,
        find::{short_id, short_len},
        gtd::{GtdState, GtdView},
        list::sort_key,
    },
};

/// The rendering options every view takes.
#[derive(Debug, Default, Parser)]
pub struct GtdViewArgs {
    /// Maximum width of the rendered table, in terminal columns.
    #[arg(long = "max-width", short = 'w', value_name = "COLUMNS")]
    pub max_width: Option<u16>,
}

/// List the Inbox: what was captured and is not clarified yet.
///
/// The open tasks carrying the `inbox` category, compared
/// case-insensitively, across every account and tasklist.
///
/// JSON output: `{"view", "tasks": [{"shortId", "account", "tasklist",
/// "id", "uid", "summary", ...}]}`.
#[derive(Debug, Parser)]
pub struct InboxCommand {
    #[command(flatten)]
    pub args: GtdViewArgs,
}

/// List the next actions.
///
/// The open tasks carrying none of `inbox`, `waiting` and `someday`
/// (a task with no status included), across every account and tasklist.
/// `--category` narrows to the tasks carrying it: a context, a project.
///
/// JSON output: as `inbox`.
#[derive(Debug, Parser)]
pub struct NextCommand {
    /// Only list the tasks carrying this category, the flag repeating:
    /// a task has to carry every one named.
    #[arg(long = "category", short = 'C', value_name = "TEXT")]
    pub categories: Vec<String>,
    #[command(flatten)]
    pub args: GtdViewArgs,
}

/// List what someone else owes: the open tasks carrying `waiting`.
///
/// JSON output: as `inbox`.
#[derive(Debug, Parser)]
pub struct WaitingCommand {
    #[command(flatten)]
    pub args: GtdViewArgs,
}

/// List what is not a commitment: the open tasks carrying `someday`.
///
/// JSON output: as `inbox`.
#[derive(Debug, Parser)]
pub struct SomedayCommand {
    #[command(flatten)]
    pub args: GtdViewArgs,
}

impl InboxCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        printer.out(view(&mut scope, GtdView::Inbox, &[], self.args.max_width)?)
    }
}

impl NextCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        let output = view(
            &mut scope,
            GtdView::Next,
            &self.categories,
            self.args.max_width,
        )?;
        printer.out(output)
    }
}

impl WaitingCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        printer.out(view(
            &mut scope,
            GtdView::Waiting,
            &[],
            self.args.max_width,
        )?)
    }
}

impl SomedayCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        printer.out(view(
            &mut scope,
            GtdView::Someday,
            &[],
            self.args.max_width,
        )?)
    }
}

/// The tasks of one view across the scope, sorted, with their short ids.
pub fn view(
    scope: &mut Scope,
    view: GtdView,
    filter: &[String],
    max_width: Option<u16>,
) -> Result<GtdViewOutput> {
    let found = scope.select(&|status, categories| view.holds(status, categories, filter))?;
    let query = view.query(filter)?;
    Ok(GtdViewOutput::new(scope, view, query, found, max_width))
}

/// One task of a view, with where it lives.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct GtdViewRow {
    /// The shortest `UID` prefix telling the listed tasks apart, which
    /// `gtd clarify` and the `task` commands take.
    pub short_id: String,
    /// The account holding the task.
    pub account: String,
    /// The tasklist holding the task.
    pub tasklist: String,
    /// The task.
    #[serde(flatten)]
    pub task: Task,
}

/// The rendered view.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct GtdViewOutput {
    #[serde(skip)]
    pub style: TableStyle,
    #[serde(skip)]
    pub arrangement: ContentArrangement,
    #[serde(skip)]
    pub max_width: Option<u16>,
    #[serde(skip)]
    multi: bool,
    #[serde(skip)]
    colors: ViewColors,
    /// The view listed.
    pub view: GtdView,
    /// The view as pimdir's guide spells it (GUIDE.md §17), for a
    /// search over a pimdir store to ask the same.
    pub query: String,
    /// The tasks, due ones first by date.
    pub tasks: Vec<GtdViewRow>,
}

/// The per-column colors a view renders with, `task list`'s.
#[derive(Clone, Copy, Debug)]
struct ViewColors {
    id: Color,
    summary: Color,
    due: Color,
    priority: Color,
    categories: Color,
}

impl GtdViewOutput {
    /// Sorts the tasks of a view and gives them their short ids.
    ///
    /// The short ids are counted over the view: a prefix too short for the
    /// whole scope is refused as ambiguous by the command it is given to,
    /// never guessed.
    pub fn new(
        scope: &Scope,
        view: GtdView,
        query: String,
        mut found: Vec<Located>,
        max_width: Option<u16>,
    ) -> Self {
        let len = short_len(found.iter().map(|found| found.task.uid.as_str()));

        found.sort_by_cached_key(|found| sort_key(&found.task));

        let account = &scope.accounts[0].client.account;

        Self {
            style: account.table_style(),
            arrangement: account.table_arrangement(),
            max_width,
            multi: scope.is_multi(),
            colors: ViewColors {
                id: account.tasks_list_table_id_color(),
                summary: account.tasks_list_table_summary_color(),
                due: account.tasks_list_table_due_color(),
                priority: account.tasks_list_table_priority_color(),
                categories: account.tasks_list_table_categories_color(),
            },
            view,
            query,
            tasks: found
                .into_iter()
                .map(|found| GtdViewRow {
                    short_id: short_id(&found.task.uid, len),
                    account: scope.name(found.account).to_owned(),
                    tasklist: found.tasklist,
                    task: found.task,
                })
                .collect(),
        }
    }
}

impl fmt::Display for GtdViewOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut header = vec![
            Cell::new("ID"),
            Cell::new("SUMMARY"),
            Cell::new("DUE"),
            Cell::new("PRIORITY"),
            Cell::new("CATEGORIES"),
            Cell::new("TASKLIST"),
        ];

        if self.multi {
            header.push(Cell::new("ACCOUNT"));
        }

        let mut table = Table::new();

        table
            .load_style(self.style)
            .set_content_arrangement(self.arrangement.clone())
            .set_header(Row::from(header))
            .add_rows(self.tasks.iter().map(|row| {
                let task = &row.task;

                // NOTE: the view already says the state, so the column
                // shows the person's own categories.
                let categories: Vec<&str> = task
                    .categories
                    .iter()
                    .map(String::as_str)
                    .filter(|category| GtdState::of(category).is_none())
                    .collect();

                let priority = task
                    .priority
                    .filter(|priority| *priority > 0)
                    .map(|priority| priority.to_string())
                    .unwrap_or_default();

                let mut cells = Row::new();
                cells.max_height(1);
                cells.add_cell(Cell::new(&row.short_id).fg(self.colors.id));
                cells.add_cell(Cell::new(&task.summary).fg(self.colors.summary));
                cells.add_cell(
                    Cell::new(task.due.as_deref().unwrap_or_default()).fg(self.colors.due),
                );
                cells.add_cell(Cell::new(priority).fg(self.colors.priority));
                cells.add_cell(Cell::new(categories.join(", ")).fg(self.colors.categories));
                cells.add_cell(Cell::new(&row.tasklist));

                if self.multi {
                    cells.add_cell(Cell::new(&row.account));
                }

                cells
            }));

        if let Some(width) = self.max_width {
            table.set_width(width);
        }

        writeln!(f)?;
        writeln!(f, "{table}")
    }
}
