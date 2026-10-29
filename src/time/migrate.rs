//! # Time migrate
//!
//! Moving every time entry of an account to the place and shape another
//! strategy writes: a copy there, then a removal here once the copy was
//! accepted, so a refused write loses nothing. A copy keeps its entry's
//! `UID` and is never written twice, so a migration cut short resumes.

use core::fmt;

use anyhow::{Context, Result};
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    config::TimeStrategy,
    scope::Scope,
    time::{
        collect,
        entry::{Shape, TimeEntry, event, journal},
        place::TimePlace,
        stop_uid,
    },
};

/// Move every time entry to the place another strategy writes.
///
/// For each account, the finished entries it keeps anywhere else (its
/// local place, its `time.collection`, in either shape) are copied where
/// `--to` writes, then removed where they were: the same bytes when the
/// shape is kept, an event made of a journal pair or the reverse
/// otherwise, the entry's `UID` kept. An entry the target holds already
/// is only removed, so a migration cut short resumes; a running entry
/// stays where it waits for its stop. Set `time.strategy` to the same
/// value afterwards, so new entries go there too.
///
/// JSON output: `{"to", "moved", "kept", "running"}`.
#[derive(Debug, Parser)]
pub struct TimeMigrateCommand {
    /// The strategy to move the entries to.
    #[arg(long, value_name = "STRATEGY")]
    pub to: TimeStrategy,
}

impl TimeMigrateCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Migrates, `now` stamping the entries a conversion writes.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        let collected = collect(&mut scope)?;
        let (mut moved, mut kept, mut running) = (0, 0, 0);

        for found in &collected.entries {
            if found.entry.end.is_none() {
                running += 1;
                continue;
            }

            let target = TimePlace::target(&mut scope, found.owner, self.to)?;
            let same_place = found.place == target.place
                && (found.place.is_local() || found.account == target.account);

            if same_place && found.entry.shape == target.shape {
                kept += 1;
                continue;
            }

            // NOTE: a calendar holds one item per UID, so a conversion in
            // place takes a UID derived from the entry's.
            let uid = match same_place {
                true => format!("{}-{}", found.entry.uid, shape_name(target.shape)),
                false => found.entry.uid.clone(),
            };

            if !collected.holds(target.account, &target.place, target.shape, &uid) {
                for contents in convert(&found.entry, target.shape, &uid, now) {
                    target
                        .place
                        .create(scope.client(target.account), contents)
                        .with_context(|| {
                            format!(
                                "Copy the time entry `{}` to {}; it was left in {}",
                                found.entry.uid,
                                target.place.describe(),
                                found.place.describe()
                            )
                        })?;
                }
            }

            for item in &found.entry.items {
                found
                    .place
                    .delete(scope.client(found.account), item)
                    .with_context(|| {
                        format!(
                            "The time entry `{}` was copied to {}, but removing it from {} \
                             failed; run the migration again to finish it",
                            found.entry.uid,
                            target.place.describe(),
                            found.place.describe()
                        )
                    })?;
            }

            moved += 1;
        }

        printer.out(TimeMigrateOutput {
            to: self.to,
            moved,
            kept,
            running,
        })
    }
}

/// How a derived `UID` names a shape.
fn shape_name(shape: Shape) -> &'static str {
    match shape {
        Shape::Event => "event",
        Shape::Journal => "journal",
    }
}

/// The items holding `entry` in `shape` under `uid`: its own bytes when
/// it has that shape, else the other shape's spelling of the same span.
pub fn convert(entry: &TimeEntry, shape: Shape, uid: &str, now: Timestamp) -> Vec<Vec<u8>> {
    if entry.shape == shape {
        return entry
            .items
            .iter()
            .map(|item| item.contents.clone())
            .collect();
    }

    match shape {
        Shape::Event => vec![event(
            uid,
            &entry.task,
            &entry.summary,
            entry.start,
            entry.end,
            now,
        )],
        Shape::Journal => {
            let mut items = vec![journal(uid, &entry.task, &entry.summary, entry.start, true)];

            if let Some(end) = entry.end {
                items.push(journal(
                    &stop_uid(uid),
                    &entry.task,
                    &entry.summary,
                    end,
                    false,
                ));
            }

            items
        }
    }
}

/// What `migrate` did.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TimeMigrateOutput {
    /// The strategy the entries moved to.
    pub to: TimeStrategy,
    /// How many entries moved.
    pub moved: usize,
    /// How many already were there.
    pub kept: usize,
    /// How many run, left where they wait for their stop.
    pub running: usize,
}

impl fmt::Display for TimeMigrateOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Moved {} time entries to the `{}` strategy, {} already there; set time.strategy \
             = \"{}\" so new entries go there too",
            self.moved, self.to, self.kept, self.to
        )?;

        if self.running > 0 {
            writeln!(
                f,
                "{} running entries stay where they wait; a stop writes them there",
                self.running
            )?;
        }

        Ok(())
    }
}
