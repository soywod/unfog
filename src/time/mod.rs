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

use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result};
use jiff::{Timestamp, tz::TimeZone};
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::Scope,
    time::{
        entry::{Shape, TimeEntry, journal, parse, stop_event},
        place::TimePlace,
    },
};

/// A time entry found in a scope, with where it lives.
#[derive(Clone, Debug)]
pub struct Found {
    /// The index of the account whose client reaches the place holding
    /// it.
    pub account: usize,
    /// The index of the account whose strategy it follows: the task's,
    /// else the one whose place holds it.
    pub owner: usize,
    /// The place holding it.
    pub place: TimePlace,
    /// The entry.
    pub entry: TimeEntry,
}

/// Every time entry of a scope, and what each place holds.
#[derive(Clone, Debug, Default)]
pub struct Collected {
    /// The entries, each `UID` once.
    pub entries: Vec<Found>,
    /// Every entry met, a `UID` met twice included, by place, shape and
    /// `UID`: what a copy checks so it is never written twice.
    held: HashSet<(PlaceKey, Shape, String)>,
}

/// A place as a scope tells it apart: a collection is its account's, a
/// local place may be shared by several.
type PlaceKey = (Option<usize>, TimePlace);

fn key(account: usize, place: &TimePlace) -> PlaceKey {
    match place.is_local() {
        true => (None, place.clone()),
        false => (Some(account), place.clone()),
    }
}

impl Collected {
    /// Whether `place`, reached through the account at `account`,
    /// holds an entry of `uid` in `shape`.
    pub fn holds(&self, account: usize, place: &TimePlace, shape: Shape, uid: &str) -> bool {
        self.held
            .contains(&(key(account, place), shape, uid.to_owned()))
    }

    /// The running entries.
    pub fn running(self) -> Vec<Found> {
        self.entries
            .into_iter()
            .filter(|found| found.entry.end.is_none())
            .collect()
    }
}

/// Every time entry of every place of every account of the scope.
///
/// A place two accounts share (one local directory, one pimdir store) is
/// read once, and an entry found twice (a migration or a stop cut short)
/// counts once.
///
/// A collection is shared with other calendar data, so an entry there
/// counts only when it relates to a task of the scope; the local place
/// holds nothing but unfog's own entries, kept even once their task is
/// gone so the history stays whole. An entry of a local place several
/// accounts share follows its task's account.
pub fn collect(scope: &mut Scope) -> Result<Collected> {
    let mut seen_places: Vec<PlaceKey> = Vec::new();
    let mut seen_entries = HashSet::new();
    let mut tasks: Option<HashMap<String, usize>> = None;
    let mut collected = Collected::default();
    let spanned = scope.accounts.iter().filter(|a| !a.extra).count();

    for index in 0..scope.accounts.len() {
        if scope.accounts[index].extra {
            continue;
        }

        for (account, place) in TimePlace::every(scope, index)? {
            let place_key = key(account, &place);

            if seen_places.contains(&place_key) {
                continue;
            }
            seen_places.push(place_key.clone());

            let items = place.list(scope.client(account))?;
            let shared = !place.is_local();

            if tasks.is_none() && (shared || spanned > 1) {
                let located = scope.tasks()?;
                let map = located
                    .into_iter()
                    .map(|found| (found.task.uid, found.account))
                    .collect();
                tasks = Some(map);
            }

            for entry in parse(items) {
                let task_account = tasks
                    .as_ref()
                    .and_then(|tasks| tasks.get(&entry.task).copied());

                if shared && tasks.is_some() && task_account.is_none() {
                    continue;
                }

                collected
                    .held
                    .insert((place_key.clone(), entry.shape, entry.uid.clone()));

                if !seen_entries.insert(entry.uid.clone()) {
                    continue;
                }

                collected.entries.push(Found {
                    account,
                    owner: match shared {
                        true => index,
                        false => task_account.unwrap_or(index),
                    },
                    place: place.clone(),
                    entry,
                });
            }
        }
    }

    Ok(collected)
}

/// The `UID` of the stop half of the journal pair whose start is `uid`.
pub fn stop_uid(uid: &str) -> String {
    format!("{uid}-stop")
}

/// Stops a running entry at `now`.
///
/// A running entry waits in the local place (or, written by an older
/// unfog, wherever its strategy wrote it): it is written finished where
/// its owner's strategy says, in that shape, then removed from where it
/// waited, so a sync only ever carries a finished entry. An entry already
/// waiting where the strategy writes is stopped there: an event gets its
/// `DTEND`, a journal its stop. A copy `collected` shows the target holds
/// already (a stop cut short) is not written again.
pub fn stop(
    scope: &mut Scope,
    collected: &Collected,
    found: &mut Found,
    now: Timestamp,
) -> Result<()> {
    let strategy = scope.client(found.owner).account.time_strategy();
    let target = TimePlace::target(scope, found.owner, strategy)?;
    let entry = &mut found.entry;
    let stays = !found.place.is_local() || found.place == target.place;

    if stays || entry.shape == Shape::Journal {
        let client = scope.client(found.account);

        match entry.shape {
            Shape::Event => {
                let item = &entry.items[0];
                let contents = stop_event(&item.contents, now)?;
                found.place.update(client, item, contents)?;
            }
            Shape::Journal => {
                let contents = journal(
                    &stop_uid(&entry.uid),
                    &entry.task,
                    &entry.summary,
                    now,
                    false,
                );
                found.place.create(client, contents)?;
            }
        }

        entry.end = Some(now);
        return Ok(());
    }

    let item = entry.items[0].clone();
    let finished = match target.shape {
        Shape::Event => vec![stop_event(&item.contents, now)?],
        Shape::Journal => vec![
            journal(&entry.uid, &entry.task, &entry.summary, entry.start, true),
            journal(
                &stop_uid(&entry.uid),
                &entry.task,
                &entry.summary,
                now,
                false,
            ),
        ],
    };

    if !collected.holds(target.account, &target.place, target.shape, &entry.uid) {
        for contents in finished {
            target
                .place
                .create(scope.client(target.account), contents)
                .with_context(|| {
                    format!(
                        "Write the stopped time entry `{}` to {}; it still runs in {}",
                        entry.uid,
                        target.place.describe(),
                        found.place.describe()
                    )
                })?;
        }
    }

    found
        .place
        .delete(scope.client(found.account), &item)
        .with_context(|| {
            format!(
                "The stopped time entry `{}` was written to {}, but removing it from {} \
                 failed; the next stop removes it",
                entry.uid,
                target.place.describe(),
                found.place.describe()
            )
        })?;

    entry.end = Some(now);
    entry.shape = target.shape;
    found.account = target.account;
    found.place = target.place;
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
