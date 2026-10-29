//! # Start, stop, status
//!
//! The commands tracking the work on one task at a time.

use core::fmt;

use anyhow::{Result, bail};
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::Scope,
    shared::uuid::uuid_v4,
    time::{Found, TimeEntryRow, collect, duration, entry::event, place::TimePlace, stop},
};

/// Start working on a task.
///
/// Writes a running time entry for it where the account's
/// `time.strategy` says: `local` (the default) keeps it on the device,
/// `event` and `journal` in the tasklist `time.collection` names. An
/// entry running for another task, in any account, is stopped first.
///
/// JSON output: `{"started", "stopped": [...]}`, each a time entry.
#[derive(Debug, Parser)]
pub struct StartCommand {
    /// Task to start: its handle, or a prefix of its UID.
    #[arg(value_name = "TASK")]
    pub task: String,
}

impl StartCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Starts the task at `now`.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        let task = scope.find(&self.task)?;
        let collected = collect(&mut scope)?;
        let mut running = collected.clone().running();

        if let Some(found) = running
            .iter()
            .find(|found| found.entry.task == task.task.uid)
        {
            bail!(
                "Task `{}` runs since {}; stop it first",
                task.task.uid,
                TimeEntryRow::new(&scope, found, now).local_start()
            );
        }

        let mut stopped = Vec::with_capacity(running.len());
        for found in &mut running {
            stop(&mut scope, &collected, found, now)?;
            stopped.push(TimeEntryRow::new(&scope, found, now));
        }

        // NOTE: the target is resolved first, so a strategy missing its
        // calendar refuses the start rather than the stop.
        let strategy = scope.client(task.account).account.time_strategy();
        TimePlace::target(&mut scope, task.account, strategy)?;

        let client = scope.client(task.account);
        let place = TimePlace::local(client)?;
        let contents = event(
            &uuid_v4()?,
            &task.task.uid,
            &task.task.summary,
            now,
            None,
            now,
        );
        place.create(client, contents)?;

        printer.out(StartOutput {
            started: TimeEntryRow {
                task: task.task.uid.clone(),
                summary: task.task.summary.clone(),
                start: now.to_string(),
                end: None,
                seconds: 0,
                account: scope.name(task.account).to_owned(),
                place: place.describe(),
            },
            stopped,
        })
    }
}

/// What `start` did.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct StartOutput {
    /// The entry started.
    pub started: TimeEntryRow,
    /// The entries the start stopped first.
    pub stopped: Vec<TimeEntryRow>,
}

impl fmt::Display for StartOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for entry in &self.stopped {
            writeln!(
                f,
                "Stopped `{}` after {}",
                entry.summary,
                duration(entry.seconds)
            )?;
        }

        writeln!(
            f,
            "Started `{}` at {}, kept in {}",
            self.started.summary,
            self.started.local_start(),
            self.started.place
        )
    }
}

/// Stop working.
///
/// Stops the running entry: an event gets its `DTEND`, a journal its
/// `stop`, in the place holding the start. Given a task, stops that
/// task's alone, and fails when it does not run.
///
/// JSON output: `{"stopped": [...]}`, each a time entry.
#[derive(Debug, Parser)]
pub struct StopCommand {
    /// Task to stop: its handle, or a prefix of its UID. Omit to stop
    /// whatever runs.
    #[arg(value_name = "TASK")]
    pub task: Option<String>,
}

impl StopCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Stops at `now`.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        let task = match &self.task {
            Some(query) => Some(scope.find(query)?.task.uid),
            None => None,
        };

        let collected = collect(&mut scope)?;
        let mut running: Vec<Found> = collected
            .clone()
            .running()
            .into_iter()
            .filter(|found| task.as_ref().is_none_or(|uid| found.entry.task == *uid))
            .collect();

        if let Some(uid) = &task
            && running.is_empty()
        {
            bail!("Task `{uid}` does not run");
        }

        let mut stopped = Vec::with_capacity(running.len());
        for found in &mut running {
            stop(&mut scope, &collected, found, now)?;
            stopped.push(TimeEntryRow::new(&scope, found, now));
        }

        printer.out(StopOutput { stopped })
    }
}

/// What `stop` did.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct StopOutput {
    /// The entries stopped.
    pub stopped: Vec<TimeEntryRow>,
}

impl fmt::Display for StopOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.stopped.is_empty() {
            return writeln!(f, "Nothing runs");
        }

        for entry in &self.stopped {
            writeln!(
                f,
                "Stopped `{}` after {}",
                entry.summary,
                duration(entry.seconds)
            )?;
        }

        Ok(())
    }
}

/// Show what runs, and since when.
///
/// JSON output: `{"running": [...]}`, each a time entry.
#[derive(Debug, Parser)]
pub struct StatusCommand;

impl StatusCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        let now = Timestamp::now();
        let running = collect(&mut scope)?
            .running()
            .iter()
            .map(|found| TimeEntryRow::new(&scope, found, now))
            .collect();

        printer.out(StatusOutput { running })
    }
}

/// What runs.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct StatusOutput {
    /// The running entries, one at most unless written elsewhere.
    pub running: Vec<TimeEntryRow>,
}

impl fmt::Display for StatusOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.running.is_empty() {
            return writeln!(f, "Nothing runs");
        }

        for entry in &self.running {
            writeln!(
                f,
                "`{}` runs since {} ({})",
                entry.summary,
                entry.local_start(),
                duration(entry.seconds)
            )?;
        }

        Ok(())
    }
}
