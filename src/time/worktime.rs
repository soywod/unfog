//! # Worktime
//!
//! The `worktime` report: the time spent per task and per category over
//! a span, read from every place of every account.

use std::{collections::BTreeMap, fmt};

use anyhow::{Result, bail};
use clap::Parser;
use jiff::{Timestamp, civil::Date, tz::TimeZone};
use pimalaya_cli::{
    printer::Printer,
    table::{Cell, Row, Table},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::Scope,
    shared::task::{
        find::{short_id, short_len},
        gtd::{GtdState, fold, same},
    },
    time::{collect, duration},
};

/// Report the time spent, per task and per category.
///
/// Sums every time entry of every place each account keeps them in, its
/// current strategy or not, a running one counted up to now. `--since`
/// and `--until` cut the entries to a span, `--category` keeps the tasks
/// carrying it.
///
/// JSON output: `{"since", "until", "tasks": [{"uid", "shortId",
/// "summary", "categories", "seconds"}], "categories": [{"category",
/// "seconds"}], "seconds"}`.
#[derive(Debug, Parser)]
pub struct WorktimeCommand {
    /// Count from this day (YYYY-MM-DD, local midnight) or instant
    /// (RFC 3339).
    #[arg(long, value_name = "TIME")]
    pub since: Option<String>,
    /// Count up to this day (YYYY-MM-DD, local midnight, exclusive) or
    /// instant (RFC 3339).
    #[arg(long, value_name = "TIME")]
    pub until: Option<String>,
    /// Only count the tasks carrying this category, the flag repeating:
    /// a task has to carry every one named.
    #[arg(long = "category", short = 'C', value_name = "TEXT")]
    pub categories: Vec<String>,
}

impl WorktimeCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Reports at `now`, the end of a running entry.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        printer.out(self.report(scope, now)?)
    }

    /// The report at `now`.
    pub fn report(self, mut scope: Scope, now: Timestamp) -> Result<WorktimeOutput> {
        let since = self.since.as_deref().map(bound).transpose()?;
        let until = self.until.as_deref().map(bound).transpose()?;

        let entries = collect(&mut scope)?.entries;
        let tasks = scope.tasks()?;

        let mut per_task: BTreeMap<String, WorktimeTask> = BTreeMap::new();

        for found in &entries {
            let entry = &found.entry;
            let task = tasks.iter().find(|task| task.task.uid == entry.task);
            let categories: Vec<String> = task
                .map(|task| task.task.categories.clone())
                .unwrap_or_default();

            let carries = |wanted: &String| categories.iter().any(|known| same(known, wanted));
            if !self.categories.iter().all(carries) {
                continue;
            }

            let start = since.map_or(entry.start, |since| entry.start.max(since));
            let end = entry.end.unwrap_or(now);
            let end = until.map_or(end, |until| end.min(until));
            let seconds = end.as_second() - start.as_second();

            if seconds <= 0 {
                continue;
            }

            let row = per_task
                .entry(entry.task.clone())
                .or_insert_with(|| WorktimeTask {
                    short_id: String::new(),
                    uid: entry.task.clone(),
                    summary: task
                        .map(|task| task.task.summary.clone())
                        .unwrap_or_else(|| entry.summary.clone()),
                    categories: categories
                        .iter()
                        .filter(|category| GtdState::of(category).is_none())
                        .cloned()
                        .collect(),
                    seconds: 0,
                });
            row.seconds += seconds;
        }

        let mut tasks: Vec<WorktimeTask> = per_task.into_values().collect();
        let len = short_len(tasks.iter().map(|task| task.uid.as_str()));
        for task in &mut tasks {
            task.short_id = short_id(&task.uid, len);
        }
        tasks.sort_by(|a, b| b.seconds.cmp(&a.seconds).then(a.summary.cmp(&b.summary)));

        // NOTE: two spellings of one category are one row, shown as the
        // first met, and a task carrying both counts once in it.
        let mut per_category: BTreeMap<String, WorktimeCategory> = BTreeMap::new();
        for task in &tasks {
            let mut counted = Vec::new();

            for category in &task.categories {
                let folded = fold(category);
                if counted.contains(&folded) {
                    continue;
                }

                per_category
                    .entry(folded.clone())
                    .or_insert_with(|| WorktimeCategory {
                        category: category.clone(),
                        seconds: 0,
                    })
                    .seconds += task.seconds;
                counted.push(folded);
            }
        }

        let mut categories: Vec<WorktimeCategory> = per_category.into_values().collect();
        categories.sort_by(|a, b| b.seconds.cmp(&a.seconds).then(a.category.cmp(&b.category)));

        Ok(WorktimeOutput {
            since: since.map(|since| since.to_string()),
            until: until.map(|until| until.to_string()),
            seconds: tasks.iter().map(|task| task.seconds).sum(),
            tasks,
            categories,
        })
    }
}

/// Reads a bound: a day at local midnight, or an RFC 3339 instant.
fn bound(value: &str) -> Result<Timestamp> {
    if let Ok(day) = value.parse::<Date>() {
        return Ok(day.to_zoned(TimeZone::system())?.timestamp());
    }

    match value.parse::<Timestamp>() {
        Ok(at) => Ok(at),
        Err(_) => bail!("Cannot read time `{value}`; give YYYY-MM-DD or an RFC 3339 instant"),
    }
}

/// The time spent on one task.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct WorktimeTask {
    /// The shortest `UID` prefix telling the reported tasks apart.
    pub short_id: String,
    /// The task's `UID`.
    pub uid: String,
    /// Its summary.
    pub summary: String,
    /// Its categories, the reserved ones aside.
    pub categories: Vec<String>,
    /// The time spent, in seconds.
    pub seconds: i64,
}

/// The time spent on the tasks of one category.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct WorktimeCategory {
    /// The category.
    pub category: String,
    /// The time spent, in seconds.
    pub seconds: i64,
}

/// The report.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct WorktimeOutput {
    /// Where the span starts, RFC 3339.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub since: Option<String>,
    /// Where it ends.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub until: Option<String>,
    /// Per task, the most time first.
    pub tasks: Vec<WorktimeTask>,
    /// Per category, the most time first; a task counts in each of its
    /// categories.
    pub categories: Vec<WorktimeCategory>,
    /// The time spent in all, in seconds.
    pub seconds: i64,
}

impl fmt::Display for WorktimeOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tasks = Table::new();
        tasks
            .set_header(Row::from(vec![
                Cell::new("ID"),
                Cell::new("SUMMARY"),
                Cell::new("CATEGORIES"),
                Cell::new("WORKTIME"),
            ]))
            .add_rows(self.tasks.iter().map(|task| {
                Row::from(vec![
                    Cell::new(&task.short_id),
                    Cell::new(&task.summary),
                    Cell::new(task.categories.join(", ")),
                    Cell::new(duration(task.seconds)),
                ])
            }));

        writeln!(f)?;
        writeln!(f, "{tasks}")?;

        if !self.categories.is_empty() {
            let mut categories = Table::new();
            categories
                .set_header(Row::from(vec![
                    Cell::new("CATEGORY"),
                    Cell::new("WORKTIME"),
                ]))
                .add_rows(self.categories.iter().map(|category| {
                    Row::from(vec![
                        Cell::new(&category.category),
                        Cell::new(duration(category.seconds)),
                    ])
                }));

            writeln!(f)?;
            writeln!(f, "{categories}")?;
        }

        writeln!(f)?;
        writeln!(f, "Total: {}", duration(self.seconds))
    }
}
