//! Time tracking over offline fixtures: vdir homes and pimdir stores in
//! temporary directories.

use clap::Parser;
use jiff::Timestamp;

use crate::{
    config::Config,
    testing::Kept,
    time::{
        collect,
        migrate::TimeMigrateCommand,
        track::{StartCommand, StopCommand},
        worktime::WorktimeCommand,
    },
};

fn at(time: &str) -> Timestamp {
    format!("2026-10-28T{time}:00Z").parse().unwrap()
}

fn start(config: &Config, task: &str, time: &str) -> serde_json::Value {
    let mut printer = Kept::default();
    StartCommand::try_parse_from(["start", task])
        .unwrap()
        .execute_at(&mut printer, crate::testing::scope(config), at(time))
        .unwrap();
    printer.last().clone()
}

fn stop(config: &Config, time: &str) -> serde_json::Value {
    let mut printer = Kept::default();
    StopCommand::try_parse_from(["stop"])
        .unwrap()
        .execute_at(&mut printer, crate::testing::scope(config), at(time))
        .unwrap();
    printer.last().clone()
}

fn worktime(config: &Config, args: &[&str]) -> serde_json::Value {
    let report = WorktimeCommand::try_parse_from([&["worktime"], args].concat())
        .unwrap()
        .report(crate::testing::scope(config), at("23:00"))
        .unwrap();
    serde_json::to_value(report).unwrap()
}

/// The tasks whose entries run, by `UID`.
fn running(config: &Config) -> Vec<String> {
    let mut scope = crate::testing::scope(config);
    let mut uids: Vec<String> = collect(&mut scope)
        .unwrap()
        .into_iter()
        .filter(|found| found.entry.end.is_none())
        .map(|found| found.entry.task)
        .collect();
    uids.sort();
    uids
}

#[cfg(feature = "vdir")]
mod vdir {
    use std::{fs, path::Path};

    use super::*;
    use crate::{
        config::TimeStrategy,
        testing::{config, gtd_fixture, timed, vdir_account, vdir_items},
    };

    /// An account over the GTD fixture, its time kept by `strategy`, its
    /// `time.collection` the tasklist `time`, its local directory beside.
    fn account(dir: &Path, strategy: TimeStrategy) -> crate::config::AccountConfig {
        let home = dir.join("home");
        let account = vdir_account(&home, &["tasks", "time"]);
        if fs::read_dir(home.join("tasks")).unwrap().next().is_none() {
            vdir_items(&home, "tasks", &gtd_fixture());
        }
        timed(account, strategy, Some("time"), &dir.join("local"))
    }

    fn fixture(dir: &Path, strategy: TimeStrategy) -> Config {
        config(vec![("personal", account(dir, strategy))])
    }

    /// The bodies a directory holds.
    fn bodies(dir: &Path) -> Vec<String> {
        match fs::read_dir(dir) {
            Ok(entries) => entries
                .map(|entry| fs::read_to_string(entry.unwrap().path()).unwrap())
                .collect(),
            Err(_) => Vec::new(),
        }
    }

    #[test]
    fn each_strategy_round_trips() {
        for strategy in [
            TimeStrategy::Local,
            TimeStrategy::Event,
            TimeStrategy::Journal,
        ] {
            let dir = tempfile::tempdir().unwrap();
            let config = fixture(dir.path(), strategy);

            let started = start(&config, "next-1", "09:00");
            assert_eq!(started["started"]["task"], "next-1", "{strategy}");
            assert_eq!(running(&config), ["next-1"], "{strategy}");

            let stopped = stop(&config, "10:30");
            assert_eq!(stopped["stopped"][0]["seconds"], 5400, "{strategy}");
            assert!(running(&config).is_empty(), "{strategy}");

            let report = worktime(&config, &[]);
            assert_eq!(report["seconds"], 5400, "{strategy}");
            assert_eq!(report["tasks"][0]["uid"], "next-1", "{strategy}");
            assert_eq!(report["categories"][0]["category"], "home", "{strategy}");

            let local = bodies(&dir.path().join("local"));
            let synced = bodies(&dir.path().join("home/time"));

            match strategy {
                TimeStrategy::Local => {
                    assert_eq!(local.len(), 1);
                    assert!(synced.is_empty());
                    assert!(local[0].contains("BEGIN:VEVENT\r\n"), "{}", local[0]);
                    assert!(
                        local[0].contains("DTEND:20261028T103000Z\r\n"),
                        "{}",
                        local[0]
                    );
                    assert!(local[0].contains("TRANSP:TRANSPARENT\r\n"), "{}", local[0]);
                    assert!(local[0].contains("RELATED-TO:next-1\r\n"), "{}", local[0]);
                }
                TimeStrategy::Event => {
                    assert!(local.is_empty());
                    assert_eq!(synced.len(), 1);
                    assert!(synced[0].contains("DTEND:20261028T103000Z\r\n"));
                }
                TimeStrategy::Journal => {
                    assert!(local.is_empty());
                    assert_eq!(synced.len(), 2);
                    assert!(
                        synced
                            .iter()
                            .all(|body| body.contains("BEGIN:VJOURNAL\r\n"))
                    );
                    assert!(
                        synced
                            .iter()
                            .any(|body| body.contains("CATEGORIES:start\r\n"))
                    );
                    assert!(
                        synced
                            .iter()
                            .any(|body| body.contains("CATEGORIES:stop\r\n"))
                    );
                }
            }
        }
    }

    /// One entry runs per person: a start in one account stops what runs
    /// in another, whatever their strategies.
    #[test]
    fn a_start_stops_the_running_entry() {
        let dir = tempfile::tempdir().unwrap();
        let other = tempfile::tempdir().unwrap();
        let config = config(vec![
            ("personal", account(dir.path(), TimeStrategy::Local)),
            ("work", {
                let home = other.path().join("home");
                let account = vdir_account(&home, &["tasks", "time"]);
                vdir_items(
                    &home,
                    "tasks",
                    &[crate::testing::task("work-1", "SUMMARY:Review the PR\r\n")],
                );
                timed(
                    account,
                    TimeStrategy::Journal,
                    Some("time"),
                    &other.path().join("local"),
                )
            }),
        ]);

        start(&config, "next-1", "09:00");
        let started = start(&config, "work-1", "10:00");

        assert_eq!(started["stopped"][0]["task"], "next-1");
        assert_eq!(started["stopped"][0]["seconds"], 3600);
        assert_eq!(running(&config), ["work-1"]);

        let mut printer = Kept::default();
        let again = StartCommand::try_parse_from(["start", "work-1"])
            .unwrap()
            .execute_at(&mut printer, crate::testing::scope(&config), at("10:05"));
        assert!(again.unwrap_err().to_string().contains("runs since"));

        let stopped = stop(&config, "10:30");
        assert_eq!(stopped["stopped"][0]["task"], "work-1");
        assert_eq!(worktime(&config, &[])["seconds"], 5400);
    }

    /// The entries written under three strategies, one after another.
    fn three_strategies(dir: &Path) -> Config {
        let local = fixture(dir, TimeStrategy::Local);
        start(&local, "next-1", "09:00");
        stop(&local, "10:00");

        let event = fixture(dir, TimeStrategy::Event);
        start(&event, "next-2", "11:00");
        stop(&event, "11:30");

        let journal = fixture(dir, TimeStrategy::Journal);
        start(&journal, "next-1", "14:00");
        stop(&journal, "14:15");

        journal
    }

    #[test]
    fn worktime_sums_across_strategies() {
        let dir = tempfile::tempdir().unwrap();
        let config = three_strategies(dir.path());

        let report = worktime(&config, &[]);
        assert_eq!(report["seconds"], 3600 + 1800 + 900);
        assert_eq!(report["tasks"][0]["uid"], "next-1");
        assert_eq!(report["tasks"][0]["seconds"], 4500);
        assert_eq!(report["tasks"][1]["seconds"], 1800);

        let categories: Vec<(String, i64)> = report["categories"]
            .as_array()
            .unwrap()
            .iter()
            .map(|row| {
                (
                    row["category"].as_str().unwrap().to_owned(),
                    row["seconds"].as_i64().unwrap(),
                )
            })
            .collect();
        assert_eq!(
            categories,
            [
                ("home".to_owned(), 4500),
                ("q3".to_owned(), 1800),
                ("work".to_owned(), 1800)
            ]
        );

        assert_eq!(worktime(&config, &["-C", "Work"])["seconds"], 1800);
        assert_eq!(
            worktime(
                &config,
                &[
                    "--since",
                    "2026-10-28T09:30:00Z",
                    "--until",
                    "2026-10-28T14:05:00Z"
                ]
            )["seconds"],
            1800 + 1800 + 300
        );
    }

    #[test]
    fn a_migration_moves_every_entry_and_keeps_the_worktime() {
        let dir = tempfile::tempdir().unwrap();
        let config = three_strategies(dir.path());
        let before = worktime(&config, &[]);

        let mut printer = Kept::default();
        TimeMigrateCommand::try_parse_from(["migrate", "--to", "event"])
            .unwrap()
            .execute_at(&mut printer, crate::testing::scope(&config), at("22:00"))
            .unwrap();
        assert_eq!(printer.last()["moved"], 2);
        assert_eq!(printer.last()["kept"], 1);

        assert!(bodies(&dir.path().join("local")).is_empty());
        let synced = bodies(&dir.path().join("home/time"));
        assert_eq!(synced.len(), 3);
        assert!(synced.iter().all(|body| body.contains("BEGIN:VEVENT\r\n")));

        let after = worktime(&config, &[]);
        assert_eq!(after["seconds"], before["seconds"]);
        assert_eq!(after["tasks"], before["tasks"]);
    }

    #[test]
    fn the_event_strategy_needs_a_collection() {
        let dir = tempfile::tempdir().unwrap();
        let mut config = fixture(dir.path(), TimeStrategy::Event);
        config.accounts.get_mut("personal").unwrap().time.collection = None;

        let err = StartCommand::try_parse_from(["start", "next-1"])
            .unwrap()
            .execute_at(
                &mut Kept::default(),
                crate::testing::scope(&config),
                at("09:00"),
            )
            .unwrap_err();
        assert!(err.to_string().contains("time.collection"), "{err}");
    }
}

#[cfg(feature = "pimdir")]
mod pimdir {
    use io_pimdir::client::{TIME_COLLECTION, reader::PimdirReader};

    use super::*;
    use crate::{
        config::TimeStrategy,
        testing::{config, gtd_fixture, pimdir_account, timed},
    };

    /// The local strategy on pimdir keeps its entries in the store's local
    /// time collection, written by the owner and never queued for a sync.
    #[test]
    fn the_local_strategy_writes_the_pimdir_time_collection() {
        let dir = tempfile::tempdir().unwrap();
        let account = pimdir_account(dir.path(), &["cal"], &gtd_fixture());
        let config = config(vec![(
            "personal",
            timed(
                account,
                TimeStrategy::Local,
                None,
                &dir.path().join("unused"),
            ),
        )]);

        start(&config, "next-1", "09:00");
        assert_eq!(running(&config), ["next-1"]);
        stop(&config, "10:30");

        let reader = PimdirReader::open(dir.path()).unwrap();
        let items = reader.list_summaries(TIME_COLLECTION, None, 10).unwrap();
        assert_eq!(items.len(), 1);
        assert!(reader.list_pending_actions().unwrap().is_empty());
        assert!(
            reader
                .collection_sources(TIME_COLLECTION)
                .unwrap()
                .is_empty()
        );
        let body = reader
            .blobs()
            .get(items[0].object.as_ref().unwrap())
            .unwrap()
            .unwrap();
        let body = String::from_utf8(body).unwrap();
        assert!(body.contains("DTEND:20261028T103000Z\r\n"), "{body}");
        assert!(!dir.path().join("unused").exists());

        assert_eq!(worktime(&config, &[])["seconds"], 5400);

        // NOTE: a migration to the directory-less journal strategy needs a
        // collection; the local time collection is never a tasklist.
        let mut config = config;
        config.accounts.get_mut("personal").unwrap().time.collection = Some("cal".into());
        TimeMigrateCommand::try_parse_from(["migrate", "--to", "journal"])
            .unwrap()
            .execute_at(
                &mut Kept::default(),
                crate::testing::scope(&config),
                at("22:00"),
            )
            .unwrap();

        let reader = PimdirReader::open(dir.path()).unwrap();
        assert!(
            reader
                .list_summaries(TIME_COLLECTION, None, 10)
                .unwrap()
                .is_empty()
        );
        assert_eq!(reader.pending_creates("cal").unwrap().len(), 2);
    }
}

#[test]
fn a_duration_reads_as_hours_and_minutes() {
    use crate::time::duration;

    assert_eq!(duration(59), "0m");
    assert_eq!(duration(45 * 60), "45m");
    assert_eq!(duration(2 * 3600 + 5 * 60 + 30), "2h05");
}
