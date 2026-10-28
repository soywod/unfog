//! # Test support
//!
//! Offline fixtures the command tests share: a printer keeping what it
//! is handed as JSON, tasks in the shape another client writes them,
//! vdir homes and pimdir stores in temporary directories.

use core::fmt;
use std::{collections::HashMap, path::Path};

use anyhow::Result;
use pimalaya_cli::printer::Printer;
use serde::Serialize;

use crate::{
    backend::Backend,
    config::{AccountConfig, Config, TimeConfig, TimeStrategy},
    scope::Scope,
};

/// A printer keeping every output as JSON.
#[derive(Default)]
pub struct Kept {
    pub outputs: Vec<serde_json::Value>,
}

impl Printer for Kept {
    fn out<T: fmt::Display + Serialize>(&mut self, data: T) -> Result<()> {
        self.outputs.push(serde_json::to_value(data)?);
        Ok(())
    }

    fn is_json(&self) -> bool {
        true
    }
}

impl Kept {
    /// The last output.
    pub fn last(&self) -> &serde_json::Value {
        self.outputs.last().expect("an output")
    }
}

/// A VTODO of `uid`, its properties `lines` (each ending in CRLF), as a
/// client other than unfog writes it.
pub fn task(uid: &str, lines: &str) -> String {
    format!(
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//other//client//EN\r\nBEGIN:VTODO\r\n\
         UID:{uid}\r\nDTSTAMP:20261001T080000Z\r\n{lines}X-OTHER-CLIENT:kept\r\n\
         END:VTODO\r\nEND:VCALENDAR\r\n"
    )
}

/// The GTD fixture: one task per view, a cased `Inbox`, closed tasks
/// that are in none, and an event that is no task.
pub fn gtd_fixture() -> Vec<String> {
    vec![
        task("inbox-1", "SUMMARY:Call Jo\r\nCATEGORIES:Inbox\r\n"),
        task("next-1", "SUMMARY:Paint the hallway\r\nCATEGORIES:home\r\n"),
        task(
            "next-2",
            "SUMMARY:Write the report\r\nSTATUS:IN-PROCESS\r\nCATEGORIES:work,q3\r\n",
        ),
        task(
            "waiting-1",
            "SUMMARY:Quote from the plumber\r\nSTATUS:NEEDS-ACTION\r\n\
             CATEGORIES:home,WAITING\r\n",
        ),
        task(
            "someday-1",
            "SUMMARY:Learn the cello\r\nCATEGORIES:someday\r\n",
        ),
        task(
            "done-1",
            "SUMMARY:Old capture\r\nSTATUS:COMPLETED\r\nCATEGORIES:inbox\r\n",
        ),
        task("cancelled-1", "SUMMARY:Dropped\r\nSTATUS:CANCELLED\r\n"),
        String::from(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//other//client//EN\r\n\
             BEGIN:VEVENT\r\nUID:event-1\r\nDTSTAMP:20261001T080000Z\r\n\
             DTSTART:20261005T090000Z\r\nSUMMARY:Not a task\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n",
        ),
    ]
}

/// A vdir account over `home`, its tasklists the directories named.
#[cfg(feature = "vdir")]
pub fn vdir_account(home: &Path, tasklists: &[&str]) -> AccountConfig {
    for tasklist in tasklists {
        std::fs::create_dir_all(home.join(tasklist)).unwrap();
    }

    AccountConfig {
        vdir: Some(crate::config::VdirConfig {
            home_dir: home.to_path_buf(),
        }),
        ..Default::default()
    }
}

/// Writes `bodies` into the vdir tasklist `tasklist` of `home`, one file
/// each named after its index.
#[cfg(feature = "vdir")]
pub fn vdir_items(home: &Path, tasklist: &str, bodies: &[String]) {
    for (index, body) in bodies.iter().enumerate() {
        std::fs::write(home.join(tasklist).join(format!("{index}.ics")), body).unwrap();
    }
}

/// A pimdir store at `root` holding the calendar collections named, the
/// first carrying `bodies` as items a sync applied, and an account over
/// it.
#[cfg(feature = "pimdir")]
pub fn pimdir_account(root: &Path, collections: &[&str], bodies: &[String]) -> AccountConfig {
    use crate::{config::PimdirConfig, pimdir::backend::PimdirBackend};

    let config = || PimdirConfig {
        root: root.to_path_buf(),
        account: None,
    };

    {
        let store = io_pimdir::client::PimdirStore::open(root).unwrap();
        for collection in collections {
            store
                .ensure_collection(*collection, "text/calendar")
                .unwrap();
        }
    }

    let mut backend = PimdirBackend::new(config()).unwrap();
    for body in bodies {
        backend
            .create_item(collections[0], body.as_bytes().to_vec())
            .unwrap();
    }
    drop(backend);

    drain(root);

    AccountConfig {
        pimdir: Some(config()),
        ..Default::default()
    }
}

/// Applies the queue of the store at `root` as a sync would.
#[cfg(feature = "pimdir")]
pub fn drain(root: &Path) {
    let mut owner = io_pimdir::client::PimdirStore::open(root)
        .unwrap()
        .for_source("caldav");
    owner.drain().unwrap();
}

/// Sets the time options of an account.
pub fn timed(
    mut account: AccountConfig,
    strategy: TimeStrategy,
    collection: Option<&str>,
    directory: &Path,
) -> AccountConfig {
    account.time = TimeConfig {
        strategy: Some(strategy),
        collection: collection.map(Into::into),
        directory: Some(directory.to_path_buf()),
    };
    account
}

/// A configuration holding `accounts`, the first the default.
pub fn config(accounts: Vec<(&str, AccountConfig)>) -> Config {
    let mut map = HashMap::new();

    for (index, (name, mut account)) in accounts.into_iter().enumerate() {
        account.default = index == 0;
        map.insert(name.to_owned(), account);
    }

    Config {
        accounts: map,
        ..Default::default()
    }
}

/// The scope of every account of `config`.
pub fn scope(config: &Config) -> Scope {
    Scope::new(config.clone(), None, Backend::Auto).unwrap()
}

/// The `uid`s of a JSON list of tasks under `key`, sorted.
pub fn uids(value: &serde_json::Value, key: &str, field: &str) -> Vec<String> {
    let mut uids: Vec<String> = value[key]
        .as_array()
        .unwrap_or_else(|| panic!("an array under {key}: {value}"))
        .iter()
        .map(|row| row[field].as_str().unwrap().to_owned())
        .collect();
    uids.sort();
    uids
}
