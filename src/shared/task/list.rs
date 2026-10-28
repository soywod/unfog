//! # Task list
//!
//! The `task list` command, rendering the VTODOs of a tasklist as a
//! table.

use std::fmt;

use anyhow::Result;
use chrono::NaiveDate;
use clap::Parser;
use ical::component::IcalComponentKind;
use pimalaya_cli::{
    printer::Printer,
    table::{Cell, Color, ContentArrangement, Row, Table, TableStyle},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{
    arg::TasklistIdArg,
    client::TaskClient,
    item::TaskItemQuery,
    task::{
        Task,
        fields::TaskStatusArg,
        find::{short_id, short_len},
    },
};

/// List the tasks of a tasklist.
///
/// Open tasks (any status but `COMPLETED` and `CANCELLED`, no status
/// included) come before closed ones, and within each group the
/// tasks with a due date come first, earliest first. The ID column is
/// the shortest `UID` prefix telling the listed tasks apart, which every
/// other command takes.
///
/// JSON output: `{"tasks": [{"id", "uid", "shortId", "summary", "due",
/// "status", "priority", "categories", ...}]}`.
#[derive(Debug, Parser)]
pub struct TaskListCommand {
    #[command(flatten)]
    pub tasklist: TasklistIdArg,
    /// Only list the tasks of this status, the flag repeating.
    #[arg(long, value_name = "STATUS")]
    pub status: Vec<TaskStatusArg>,
    /// Only list the tasks due before this day (exclusive, YYYY-MM-DD).
    #[arg(long, value_name = "DATE")]
    pub due_before: Option<NaiveDate>,
    /// Only list the tasks carrying this category, the flag repeating:
    /// a task has to carry every one named.
    #[arg(long = "category", value_name = "TEXT")]
    pub categories: Vec<String>,
    /// Maximum width of the rendered table, in terminal columns.
    #[arg(long = "max-width", short = 'w', value_name = "COLUMNS")]
    pub max_width: Option<u16>,
}

impl TaskListCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        let tasklist_id = client.account.tasklist_id(self.tasklist.id.clone())?;

        let query = TaskItemQuery {
            kind: Some(IcalComponentKind::VTodo),
            ..Default::default()
        };

        let items = client.list_items(&tasklist_id, query)?;
        let all: Vec<Task> = items.iter().flat_map(Task::project).collect();

        // NOTE: the short ids are counted over the whole tasklist rather
        // than over what the filters keep, so a short id read from a
        // filtered listing still names one task in the next command.
        let len = short_len(all.iter().map(|task| task.uid.as_str()));

        let mut tasks: Vec<Task> = all.into_iter().filter(|task| self.keeps(task)).collect();

        sort(&mut tasks);

        let tasks = tasks
            .into_iter()
            .map(|task| TaskListRow {
                short_id: short_id(&task.uid, len),
                task,
            })
            .collect();

        printer.out(TaskListOutput {
            style: client.account.table_style(),
            arrangement: client.account.table_arrangement(),
            max_width: self.max_width,
            colors: TaskColors {
                id: client.account.tasks_list_table_id_color(),
                summary: client.account.tasks_list_table_summary_color(),
                due: client.account.tasks_list_table_due_color(),
                status: client.account.tasks_list_table_status_color(),
                priority: client.account.tasks_list_table_priority_color(),
                categories: client.account.tasks_list_table_categories_color(),
            },
            tasks,
        })
    }

    /// Whether `task` passes every filter given.
    fn keeps(&self, task: &Task) -> bool {
        if !self.status.is_empty() {
            let status = task.status.as_deref().unwrap_or("NEEDS-ACTION");

            if !self
                .status
                .iter()
                .any(|wanted| wanted.as_status().eq_ignore_ascii_case(status))
            {
                return false;
            }
        }

        if let Some(day) = self.due_before {
            let before = day.format("%Y%m%dT000000").to_string();

            if !task.due_key().is_some_and(|due| due < before) {
                return false;
            }
        }

        self.categories.iter().all(|wanted| {
            task.categories
                .iter()
                .any(|category| category.eq_ignore_ascii_case(wanted))
        })
    }
}

/// Orders tasks open first, then due ones by date, then undated ones,
/// keeping the backend's order among equals.
fn sort(tasks: &mut [Task]) {
    tasks.sort_by_cached_key(sort_key);
}

/// What a listing orders a task by: open first, then due ones by date,
/// then undated ones.
pub(crate) fn sort_key(task: &Task) -> (bool, bool, Option<String>) {
    let due = task.due_key();
    (!task.is_open(), due.is_none(), due)
}

/// The per-column colors a task listing renders with.
#[derive(Clone, Copy, Debug)]
struct TaskColors {
    id: Color,
    summary: Color,
    due: Color,
    status: Color,
    priority: Color,
    categories: Color,
}

/// One listed task, with the short id the listing addresses it by.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskListRow {
    /// The shortest `UID` prefix telling the listed tasks apart.
    pub short_id: String,
    /// The task.
    #[serde(flatten)]
    pub task: Task,
}

/// The rendered task listing.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskListOutput {
    #[serde(skip)]
    pub style: TableStyle,
    #[serde(skip)]
    pub arrangement: ContentArrangement,
    #[serde(skip)]
    pub max_width: Option<u16>,
    #[serde(skip)]
    colors: TaskColors,
    /// The tasks, open first, then by due date.
    pub tasks: Vec<TaskListRow>,
}

impl fmt::Display for TaskListOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut table = Table::new();

        table
            .load_style(self.style)
            .set_content_arrangement(self.arrangement.clone())
            .set_header(Row::from(vec![
                Cell::new("ID"),
                Cell::new("SUMMARY"),
                Cell::new("DUE"),
                Cell::new("STATUS"),
                Cell::new("PRIORITY"),
                Cell::new("CATEGORIES"),
            ]))
            .add_rows(self.tasks.iter().map(|row| {
                let task = &row.task;
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
                cells.add_cell(
                    Cell::new(task.status.as_deref().unwrap_or_default()).fg(self.colors.status),
                );
                cells.add_cell(Cell::new(priority).fg(self.colors.priority));
                cells.add_cell(Cell::new(task.categories.join(", ")).fg(self.colors.categories));
                cells
            }));

        if let Some(width) = self.max_width {
            table.set_width(width);
        }

        writeln!(f)?;
        writeln!(f, "{table}")
    }
}

#[cfg(test)]
mod tests {
    use crate::shared::task::{Task, list::sort};

    fn task(uid: &str, status: Option<&str>, due: Option<&str>) -> Task {
        Task {
            uid: uid.into(),
            status: status.map(Into::into),
            due: due.map(Into::into),
            ..Default::default()
        }
    }

    #[test]
    fn open_tasks_come_first_then_due_ones_by_date() {
        let mut tasks = vec![
            task("undated", None, None),
            task("done", Some("COMPLETED"), Some("20261001")),
            task("third", Some("NEEDS-ACTION"), Some("20261003")),
            task("second", Some("IN-PROCESS"), Some("20261002T090000Z")),
            task("cancelled", Some("CANCELLED"), None),
        ];

        sort(&mut tasks);

        let order: Vec<&str> = tasks.iter().map(|task| task.uid.as_str()).collect();
        assert_eq!(order, ["second", "third", "undated", "done", "cancelled"]);
    }
}
