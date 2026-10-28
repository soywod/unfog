//! # Time migrate
//!
//! Moving every time entry of an account to the place and shape another
//! strategy writes: a copy there, then a removal here once the copy was
//! accepted, so a refused write loses nothing.

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
    shared::uuid::uuid_v4,
    time::{
        collect,
        entry::{Shape, TimeEntry, event, journal},
        place::TimePlace,
    },
};

/// Move every time entry to the place another strategy writes.
///
/// For each account, the entries it keeps anywhere else (its local
/// place, its `time.collection`, in either shape) are copied where
/// `--to` writes, then removed where they were: the same bytes when the
/// shape is kept, an event made of a journal pair or the reverse
/// otherwise. Set `time.strategy` to the same value afterwards, so new
/// entries go there too.
///
/// JSON output: `{"to", "moved", "kept"}`.
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
        let entries = collect(&mut scope)?;
        let (mut moved, mut kept) = (0, 0);

        for found in entries {
            let client = scope.client(found.account);
            let (target, shape) = TimePlace::of(client, self.to)?;

            if found.place == target && found.entry.shape == shape {
                kept += 1;
                continue;
            }

            for contents in convert(&found.entry, shape, now)? {
                target.create(client, contents).with_context(|| {
                    format!(
                        "Copy the time entry `{}` to {}; it was left in {}",
                        found.entry.uid,
                        target.describe(),
                        found.place.describe()
                    )
                })?;
            }

            for item in &found.entry.items {
                found.place.delete(client, item).with_context(|| {
                    format!(
                        "The time entry `{}` was copied to {}, but removing it from {} failed",
                        found.entry.uid,
                        target.describe(),
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
        })
    }
}

/// The items holding `entry` in `shape`: its own bytes when it has that
/// shape, else the other shape's spelling of the same span.
pub fn convert(entry: &TimeEntry, shape: Shape, now: Timestamp) -> Result<Vec<Vec<u8>>> {
    if entry.shape == shape {
        return Ok(entry
            .items
            .iter()
            .map(|item| item.contents.clone())
            .collect());
    }

    let items = match shape {
        Shape::Event => vec![event(
            &uuid_v4()?,
            &entry.task,
            &entry.summary,
            entry.start,
            entry.end,
            now,
        )],
        Shape::Journal => {
            let mut items = vec![journal(
                &uuid_v4()?,
                &entry.task,
                &entry.summary,
                entry.start,
                true,
            )];

            if let Some(end) = entry.end {
                items.push(journal(
                    &uuid_v4()?,
                    &entry.task,
                    &entry.summary,
                    end,
                    false,
                ));
            }

            items
        }
    };

    Ok(items)
}

/// What `time migrate` did.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TimeMigrateOutput {
    /// The strategy the entries moved to.
    pub to: TimeStrategy,
    /// How many entries moved.
    pub moved: usize,
    /// How many already were there.
    pub kept: usize,
}

impl fmt::Display for TimeMigrateOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Moved {} time entries to the `{}` strategy, {} already there; set time.strategy \
             = \"{}\" so new entries go there too",
            self.moved, self.to, self.kept, self.to
        )
    }
}
