//! # Time tracking
//!
//! `task start`, `task stop`, `task status`, `worktime` and `migrate`:
//! time spent on a task, kept as time entries tied to it by `RELATED-TO`
//! in one of the shapes pimdir's guide describes (GUIDE.md §18), where
//! the account's `time.strategy` says ([`place`]).
//!
//! One entry runs at a time across every account, as in unfog 1.x: a
//! start stops the running one first. Every place an account keeps
//! entries in is read, whatever its strategy now, so a history survives a
//! switch.

pub mod entry;
pub mod migrate;
pub mod place;
pub mod track;
pub mod worktime;

use std::collections::HashSet;

use anyhow::Result;
use jiff::{Timestamp, tz::TimeZone};
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::Scope,
    shared::uuid::uuid_v4,
    time::{
        entry::{Shape, TimeEntry, journal, parse, stop_event},
        place::TimePlace,
    },
};

/// A time entry found in a scope, with where it lives.
#[derive(Clone, Debug)]
pub struct Found {
    /// The index of the account whose place holds it.
    pub account: usize,
    /// The place holding it.
    pub place: TimePlace,
    /// The entry.
    pub entry: TimeEntry,
}

/// Every time entry of every place of every account of the scope.
///
/// A place two accounts share (one local directory, one pimdir store) is
/// read once, and an entry found twice (a migration cut short) counts
/// once.
pub fn collect(scope: &mut Scope) -> Result<Vec<Found>> {
    let mut seen_places: Vec<(Option<usize>, TimePlace)> = Vec::new();
    let mut seen_entries = HashSet::new();
    let mut found = Vec::new();

    for index in 0..scope.accounts.len() {
        for place in TimePlace::every(&scope.accounts[index].client)? {
            // NOTE: a collection is the account's own, a local place may
            // be shared.
            let key = match &place {
                TimePlace::Collection(_) => (Some(index), place.clone()),
                _ => (None, place.clone()),
            };

            if seen_places.contains(&key) {
                continue;
            }
            seen_places.push(key);

            let items = place.list(scope.client(index))?;

            for entry in parse(items) {
                if !seen_entries.insert(entry.uid.clone()) {
                    continue;
                }

                found.push(Found {
                    account: index,
                    place: place.clone(),
                    entry,
                });
            }
        }
    }

    Ok(found)
}

/// Stops a running entry at `now`: an event gets its `DTEND`, a journal
/// its stop, both in the place holding the start.
pub fn stop(scope: &mut Scope, found: &mut Found, now: Timestamp) -> Result<()> {
    let client = scope.client(found.account);
    let entry = &mut found.entry;

    match entry.shape {
        Shape::Event => {
            let item = &entry.items[0];
            let contents = stop_event(&item.contents, now)?;
            found.place.update(client, item, contents)?;
        }
        Shape::Journal => {
            let contents = journal(&uuid_v4()?, &entry.task, &entry.summary, now, false);
            found.place.create(client, contents)?;
        }
    }

    entry.end = Some(now);
    Ok(())
}

/// One time entry as a report shows it.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TimeEntryRow {
    /// The `UID` of the task worked on.
    pub task: String,
    /// The task's summary.
    pub summary: String,
    /// When the work started, RFC 3339 in UTC.
    pub start: String,
    /// When it stopped, absent while it runs.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end: Option<String>,
    /// How long it ran, in seconds, up to now for a running entry.
    pub seconds: i64,
    /// The account whose place holds it.
    pub account: String,
    /// Where it is kept.
    pub place: String,
}

impl TimeEntryRow {
    /// The row of `found`, a running entry counted up to `now`.
    pub fn new(scope: &Scope, found: &Found, now: Timestamp) -> Self {
        let entry = &found.entry;
        let end = entry.end.unwrap_or(now);

        Self {
            task: entry.task.clone(),
            summary: entry.summary.clone(),
            start: entry.start.to_string(),
            end: entry.end.map(|end| end.to_string()),
            seconds: end
                .as_second()
                .saturating_sub(entry.start.as_second())
                .max(0),
            account: scope.name(found.account).to_owned(),
            place: found.place.describe(),
        }
    }

    /// When the work started, in the local zone, to the minute.
    pub fn local_start(&self) -> String {
        local(&self.start)
    }
}

/// An RFC 3339 instant in the local zone, to the minute.
pub fn local(instant: &str) -> String {
    match instant.parse::<Timestamp>() {
        Ok(at) => at
            .to_zoned(TimeZone::system())
            .strftime("%Y-%m-%d %H:%M")
            .to_string(),
        Err(_) => instant.to_owned(),
    }
}

/// A duration in seconds as hours and minutes: `2h05`, `45m`.
pub fn duration(seconds: i64) -> String {
    let minutes = seconds.max(0) / 60;

    match minutes / 60 {
        0 => format!("{minutes}m"),
        hours => format!("{hours}h{:02}", minutes % 60),
    }
}

#[cfg(test)]
mod tests;
