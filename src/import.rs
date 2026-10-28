//! # Import
//!
//! The `import` command: the event log of unfog 1.x folded into one
//! VTODO per task, written to the configured backend, and its starts and
//! stops paired into time entries of the account's time strategy.
//!
//! The store is opened for reading only.

pub mod store;

use core::fmt;
use std::{collections::HashSet, fs, path::PathBuf};

use anyhow::{Context, Result};
use clap::Parser;
use ical::{
    component::{IcalComponent, IcalComponentKind, IcalComponentName},
    prop::{IcalProp, IcalPropKind, IcalPropName},
    tree::cst::IcalCst,
    value::{
        IcalValue,
        datetime::IcalDateTime,
        integer::IcalInteger,
        text::{IcalText, IcalTextList},
    },
};
use jiff::Timestamp;
use log::{info, warn};
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    config::TimeStrategy,
    import::store::{ImportEvent, ImportFormat, read_store},
    shared::{
        arg::TasklistIdArg, client::TaskClient, ical::calendar, item::TaskItemQuery, task::Task,
    },
    time::{
        entry::{Shape, event, journal, uid_of},
        place::TimePlace,
    },
};

/// Import the tasks of an unfog 1.x store.
///
/// Reads the 1.x event log (or its older v0 format) and writes one task
/// per 1.x task to the selected tasklist: the last description as its
/// summary, the last due date, the project as a plain category, done
/// tasks completed and deleted ones cancelled. A task whose UID the
/// tasklist already holds is skipped, so an import run twice writes
/// nothing the second time.
///
/// The time 1.x tracked (`start`, `stop`) becomes time entries of the
/// account's `time.strategy`, one per start and the stop that closed it,
/// each keyed on its task and its start, so a second run skips them too.
/// The store is only read.
///
/// JSON output: `{"format", "created", "skipped", "timeEntries",
/// "timeWritten", "timeSkipped", "strategy", "place"}`.
#[derive(Debug, Parser)]
pub struct ImportCommand {
    #[command(flatten)]
    pub tasklist: TasklistIdArg,
    /// The 1.x store, which 1.x keeps at `$XDG_CONFIG_HOME/unfog/store`.
    #[arg(value_name = "STORE")]
    pub store: PathBuf,
}

impl ImportCommand {
    pub fn execute(self, printer: &mut impl Printer, mut client: TaskClient) -> Result<()> {
        let tasklist_id = client.account.tasklist_id(self.tasklist.id)?;

        let contents = fs::read_to_string(&self.store)
            .with_context(|| format!("Read store {}", self.store.display()))?;

        let (format, events) = read_store(&contents)?;
        let folded = ImportFold::fold(events);

        info!(
            "read {} tasks and {} time entries from {}",
            folded.tasks.len(),
            folded.entries.len(),
            self.store.display()
        );

        let query = TaskItemQuery {
            kind: Some(IcalComponentKind::VTodo),
            ..Default::default()
        };

        let known: HashSet<String> = client
            .list_items(&tasklist_id, query)?
            .iter()
            .flat_map(Task::project)
            .map(|task| task.uid)
            .collect();

        let now = Timestamp::now();
        let (mut created, mut skipped) = (0, 0);

        for task in &folded.tasks {
            if known.contains(&task.uid) {
                skipped += 1;
                continue;
            }

            client.create_item(&tasklist_id, task.to_ical(now))?;
            created += 1;
        }

        let spans = folded.spans();
        let strategy = client.account.time_strategy();
        let (mut time_written, mut time_skipped) = (0, 0);
        let mut place = None;

        if !spans.is_empty() {
            let (target, shape) = TimePlace::of(&client, strategy)?;

            let known: HashSet<String> = target
                .list(&mut client)?
                .iter()
                .filter_map(|item| uid_of(&item.contents))
                .collect();

            for span in &spans {
                let items = span.items(shape, now);

                // NOTE: the start's item names the entry, so an entry a
                // first run wrote is skipped whole by a second one.
                if items.first().is_some_and(|(uid, _)| known.contains(uid)) {
                    time_skipped += 1;
                    continue;
                }

                for (_, contents) in items {
                    target.create(&mut client, contents)?;
                }
                time_written += 1;
            }

            place = Some(target.describe());
        }

        printer.out(ImportOutput {
            format: match format {
                ImportFormat::V1 => "1.x",
                ImportFormat::V0 => "0.x",
            },
            created,
            skipped,
            time_entries: spans.len(),
            time_written,
            time_skipped,
            strategy,
            place,
        })
    }
}

/// One span of work on a task, a start and the stop closing it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportSpan {
    /// The `UID` of the task.
    pub uid: String,
    /// The task's last description.
    pub summary: String,
    /// When it started.
    pub start: Timestamp,
    /// When it stopped, `None` when the store ends with it running.
    pub end: Option<Timestamp>,
}

impl ImportSpan {
    /// The items spelling the span in `shape`, each with its `UID`: the
    /// task's `UID` and the instant, so a second import finds them.
    pub fn items(&self, shape: Shape, now: Timestamp) -> Vec<(String, Vec<u8>)> {
        let key = |at: Timestamp| format!("{}-{}", self.uid, at.as_second());

        match shape {
            Shape::Event => {
                let uid = key(self.start);
                let contents = event(&uid, &self.uid, &self.summary, self.start, self.end, now);
                vec![(uid, contents)]
            }
            Shape::Journal => {
                let mut items = Vec::new();
                let uid = format!("{}-start", key(self.start));
                let contents = journal(&uid, &self.uid, &self.summary, self.start, true);
                items.push((uid, contents));

                if let Some(end) = self.end {
                    let uid = format!("{}-stop", key(end));
                    let contents = journal(&uid, &self.uid, &self.summary, end, false);
                    items.push((uid, contents));
                }

                items
            }
        }
    }
}

/// The tasks and the time entries of a store, its events folded.
#[derive(Debug, Default)]
pub struct ImportFold {
    /// The tasks, in the order they were added.
    pub tasks: Vec<ImportTask>,
    /// The time entries, in the order they happened.
    pub entries: Vec<ImportEntry>,
}

/// One 1.x task, its events folded.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportTask {
    /// The 1.x id, kept as the `UID`.
    pub uid: String,
    /// The last description.
    pub summary: String,
    /// The last project.
    pub project: Option<String>,
    /// The last due date.
    pub due: Option<Timestamp>,
    /// When it was added.
    pub created: Timestamp,
    /// When it last changed.
    pub modified: Timestamp,
    /// When it was done, unless undone since.
    pub done: Option<Timestamp>,
    /// Whether it was deleted, unless undeleted since.
    pub deleted: bool,
    /// When work on it started, while it runs.
    started: Option<Timestamp>,
}

/// One start or stop of the work on a task.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportEntry {
    /// When.
    pub at: Timestamp,
    /// The `UID` of the task.
    pub uid: String,
    /// Whether work started, or stopped.
    pub start: bool,
}

impl ImportFold {
    /// Applies `events` in order, as lib/Unfog/State.hs does.
    ///
    /// Done and deleted close a running task with the stop 1.x implied.
    /// An event naming a task no addition introduced is skipped.
    pub fn fold(events: Vec<ImportEvent>) -> Self {
        let mut fold = Self::default();

        for event in events {
            match event {
                ImportEvent::Added {
                    at,
                    id,
                    desc,
                    project,
                    due,
                } => fold.tasks.push(ImportTask {
                    uid: id,
                    summary: desc,
                    project,
                    due,
                    created: at,
                    modified: at,
                    done: None,
                    deleted: false,
                    started: None,
                }),
                ImportEvent::Edited {
                    at,
                    id,
                    desc,
                    project,
                    due,
                } => {
                    if let Some(task) = fold.task(&id) {
                        task.summary = desc;
                        task.project = project;
                        task.due = due;
                        task.modified = at;
                    }
                }
                ImportEvent::Started { at, id } => {
                    if let Some(task) = fold.task(&id) {
                        task.started = Some(at);
                        fold.entries.push(ImportEntry {
                            at,
                            uid: id,
                            start: true,
                        });
                    }
                }
                ImportEvent::Stopped { at, id } => fold.stop(at, &id),
                ImportEvent::Did { at, id } => {
                    fold.close(at, &id);

                    if let Some(task) = fold.task(&id) {
                        task.done = Some(at);
                        task.modified = at;
                    }
                }
                ImportEvent::Undid { at, id } => {
                    if let Some(task) = fold.task(&id) {
                        task.done = None;
                        task.modified = at;
                    }
                }
                ImportEvent::Deleted { at, id } => {
                    fold.close(at, &id);

                    if let Some(task) = fold.task(&id) {
                        task.deleted = true;
                        task.modified = at;
                    }
                }
                ImportEvent::Undeleted { at, id } => {
                    if let Some(task) = fold.task(&id) {
                        task.deleted = false;
                        task.modified = at;
                    }
                }
                ImportEvent::Context => (),
            }
        }

        fold
    }

    /// The task of `uid`, warning when there is none.
    fn task(&mut self, uid: &str) -> Option<&mut ImportTask> {
        let task = self.tasks.iter_mut().find(|task| task.uid == uid);

        if task.is_none() {
            warn!("skipping an event of task {uid}, never added");
        }

        task
    }

    /// Records a stop of `uid`.
    fn stop(&mut self, at: Timestamp, uid: &str) {
        if let Some(task) = self.task(uid) {
            task.started = None;
            self.entries.push(ImportEntry {
                at,
                uid: uid.to_owned(),
                start: false,
            });
        }
    }

    /// Records the stop a done or a deleted task implies when it runs.
    fn close(&mut self, at: Timestamp, uid: &str) {
        let running = self
            .tasks
            .iter()
            .any(|task| task.uid == uid && task.started.is_some());

        if running {
            self.stop(at, uid);
        }
    }

    /// The starts and stops paired per task in file order: a start and
    /// the stop after it, a start the store ends on left running.
    pub fn spans(&self) -> Vec<ImportSpan> {
        let mut spans: Vec<ImportSpan> = Vec::new();

        for entry in &self.entries {
            if entry.start {
                let summary = self
                    .tasks
                    .iter()
                    .find(|task| task.uid == entry.uid)
                    .map(|task| task.summary.clone())
                    .unwrap_or_default();

                spans.push(ImportSpan {
                    uid: entry.uid.clone(),
                    summary,
                    start: entry.at,
                    end: None,
                });
                continue;
            }

            if let Some(span) = spans
                .iter_mut()
                .rev()
                .find(|span| span.uid == entry.uid && span.end.is_none())
            {
                span.end = Some(entry.at);
            }
        }

        spans
    }
}

impl ImportTask {
    /// The task as a VTODO, stamped at `now`.
    pub fn to_ical(&self, now: Timestamp) -> Vec<u8> {
        let mut props = vec![
            text(IcalPropKind::Uid, &self.uid),
            stamp(IcalPropKind::DtStamp, now),
            stamp(IcalPropKind::Created, self.created),
            stamp(IcalPropKind::LastModified, self.modified),
            text(IcalPropKind::Summary, &self.summary),
        ];

        if let Some(due) = self.due {
            props.push(stamp(IcalPropKind::Due, due));
        }

        if let Some(project) = &self.project {
            props.push(categories(project));
        }

        match (self.deleted, self.done) {
            (true, _) => props.push(text(IcalPropKind::Status, "CANCELLED")),
            (false, Some(done)) => {
                props.push(text(IcalPropKind::Status, "COMPLETED"));
                props.push(stamp(IcalPropKind::Completed, done));
                props.push(IcalProp {
                    name: IcalPropName::Kind(IcalPropKind::PercentComplete),
                    params: Vec::new(),
                    value: IcalValue::Integer(IcalInteger("100".into())),
                });
            }
            (false, None) => props.push(text(IcalPropKind::Status, "NEEDS-ACTION")),
        }

        let vtodo = IcalComponent {
            name: IcalComponentName::Kind(IcalComponentKind::VTodo),
            props,
            components: Vec::new(),
        };

        IcalCst::from(calendar(vec![vtodo])).to_bytes()
    }
}

/// A text property.
fn text(kind: IcalPropKind, value: &str) -> IcalProp<'static> {
    IcalProp {
        name: IcalPropName::Kind(kind),
        params: Vec::new(),
        value: IcalValue::Text(IcalText(value.to_owned().into())),
    }
}

/// A `CATEGORIES` property holding one value.
fn categories(value: &str) -> IcalProp<'static> {
    IcalProp {
        name: IcalPropName::Kind(IcalPropKind::Categories),
        params: Vec::new(),
        value: IcalValue::TextList(IcalTextList(vec![value.to_owned().into()])),
    }
}

/// A UTC date-time property, to the second.
fn stamp(kind: IcalPropKind, at: Timestamp) -> IcalProp<'static> {
    IcalProp {
        name: IcalPropName::Kind(kind),
        params: Vec::new(),
        value: IcalValue::DateTime(IcalDateTime(
            at.strftime("%Y%m%dT%H%M%SZ").to_string().into(),
        )),
    }
}

/// What `import` did.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ImportOutput {
    /// The format the store was read in, `1.x` or `0.x`.
    pub format: &'static str,
    /// How many tasks were written.
    pub created: usize,
    /// How many tasks the tasklist already held, by `UID`.
    pub skipped: usize,
    /// How many time entries the store held, a start and its stop each.
    pub time_entries: usize,
    /// How many time entries were written.
    pub time_written: usize,
    /// How many time entries the place already held, by `UID`.
    pub time_skipped: usize,
    /// The time strategy they were written with.
    pub strategy: TimeStrategy,
    /// Where they were written, when the store held any.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub place: Option<String>,
}

impl fmt::Display for ImportOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Imported {} tasks from a {} store, {} already there",
            self.created, self.format, self.skipped
        )?;

        match &self.place {
            Some(place) => writeln!(
                f,
                "Wrote {} time entries to {place} (the `{}` strategy), {} already there",
                self.time_written, self.strategy, self.time_skipped
            ),
            None => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use jiff::Timestamp;

    use crate::{
        import::{ImportFold, store::read_store},
        shared::ical::check,
        time::entry::Shape,
    };

    const STORE: &str = include_str!("../tests/fixtures/store-1.x");

    fn fold() -> ImportFold {
        ImportFold::fold(read_store(STORE).unwrap().1)
    }

    fn at(stamp: &str) -> Timestamp {
        stamp.parse().unwrap()
    }

    #[test]
    fn edits_fold_into_one_task_carrying_the_last_values() {
        let fold = fold();
        let report = &fold.tasks[0];

        assert_eq!(fold.tasks.len(), 4);
        assert_eq!(report.uid, "4c1e0b2a-7d3e-4f00-9b1a-000000000001");
        assert_eq!(report.summary, "Write the annual report");
        assert_eq!(report.project.as_deref(), Some("work, 2020"));
        assert_eq!(report.due, Some(at("2020-12-11T17:00:00Z")));
        assert_eq!(report.done, Some(at("2020-12-03T11:30:00Z")));
        assert!(!report.deleted);
    }

    #[test]
    fn a_task_done_then_undone_then_deleted_then_undeleted_is_open() {
        let fold = fold();
        let reopened = &fold.tasks[3];

        assert_eq!(reopened.done, None);
        assert!(!reopened.deleted);
        assert!(fold.tasks[2].deleted);
    }

    #[test]
    fn done_closes_a_running_task_with_the_stop_it_implies() {
        let entries: Vec<(String, bool)> = fold()
            .entries
            .iter()
            .map(|entry| (entry.at.to_string(), entry.start))
            .collect();

        assert_eq!(
            entries,
            [
                ("2020-12-02T09:00:00Z".to_owned(), true),
                ("2020-12-02T10:00:00Z".to_owned(), false),
                ("2020-12-03T09:00:00Z".to_owned(), true),
                ("2020-12-03T11:30:00Z".to_owned(), false),
            ]
        );
    }

    #[test]
    fn a_folded_task_is_a_valid_vtodo() {
        let now = at("2026-10-28T00:00:00Z");

        for task in &fold().tasks {
            let ical = task.to_ical(now);
            assert!(check(&ical).is_empty(), "{:?}", check(&ical));
        }

        let report = String::from_utf8(fold().tasks[0].to_ical(now)).unwrap();
        assert!(report.contains("UID:4c1e0b2a-7d3e-4f00-9b1a-000000000001\r\n"));
        assert!(report.contains("SUMMARY:Write the annual report\r\n"));
        assert!(report.contains("DUE:20201211T170000Z\r\n"));
        assert!(report.contains("CATEGORIES:work\\, 2020\r\n"), "{report}");
        assert!(report.contains("STATUS:COMPLETED\r\n"));
        assert!(report.contains("COMPLETED:20201203T113000Z\r\n"));
        assert!(report.contains("PERCENT-COMPLETE:100\r\n"));

        let cancelled = String::from_utf8(fold().tasks[2].to_ical(now)).unwrap();
        assert!(cancelled.contains("STATUS:CANCELLED\r\n"));

        let escaped = String::from_utf8(fold().tasks[1].to_ical(now)).unwrap();
        assert!(
            escaped.contains("SUMMARY:Caf\u{e9} \"au lait\"\\nwith a second line\r\n"),
            "{escaped}"
        );
    }

    #[test]
    fn starts_and_stops_pair_into_spans_keyed_on_task_and_start() {
        let fold = fold();
        let spans = fold.spans();
        let report = "4c1e0b2a-7d3e-4f00-9b1a-000000000001";

        assert_eq!(spans.len(), 2);
        assert!(spans.iter().all(|span| span.uid == report));
        assert_eq!(spans[0].start, at("2020-12-02T09:00:00Z"));
        assert_eq!(spans[0].end, Some(at("2020-12-02T10:00:00Z")));
        assert_eq!(spans[1].end, Some(at("2020-12-03T11:30:00Z")));
        assert_eq!(spans[0].summary, "Write the annual report");

        let now = at("2026-10-28T00:00:00Z");
        let event = &spans[0].items(Shape::Event, now)[0];
        let text = String::from_utf8(event.1.clone()).unwrap();
        assert_eq!(event.0, format!("{report}-1606899600"));
        assert!(check(&event.1).is_empty(), "{text}");
        assert!(text.contains("DTSTART:20201202T090000Z\r\n"), "{text}");
        assert!(text.contains("DTEND:20201202T100000Z\r\n"), "{text}");
        assert!(text.contains(&format!("RELATED-TO:{report}\r\n")), "{text}");

        let journal = spans[0].items(Shape::Journal, now);
        assert_eq!(journal.len(), 2);
        let stop = String::from_utf8(journal[1].1.clone()).unwrap();
        assert!(stop.contains("CATEGORIES:stop\r\n"), "{stop}");
        assert!(check(&journal[1].1).is_empty(), "{stop}");
    }

    #[test]
    fn a_v0_store_folds_with_its_running_task_stopped_by_the_delete() {
        let store = include_str!("../tests/fixtures/store-0.x");
        let fold = ImportFold::fold(read_store(store).unwrap().1);

        assert_eq!(fold.tasks.len(), 2);
        assert_eq!(fold.tasks[0].summary, "Buy oat milk");
        assert_eq!(fold.tasks[0].project.as_deref(), Some("home"));
        assert!(fold.tasks[0].done.is_some());
        assert!(fold.tasks[1].deleted);
        assert_eq!(fold.entries.len(), 4);
        assert!(!fold.entries[3].start);
    }

    #[cfg(feature = "vdir")]
    mod vdir {
        use core::fmt;

        use anyhow::Result;
        use clap::Parser;
        use pimalaya_cli::printer::Printer;
        use serde::Serialize;

        use ical::component::IcalComponentKind;

        use crate::{
            backend::Backend,
            config::{AccountConfig, Config, TimeConfig, TimeStrategy, VdirConfig},
            import::ImportCommand,
            shared::{client::TaskClient, item::TaskItemQuery},
        };

        struct Quiet;

        impl Printer for Quiet {
            fn out<T: fmt::Display + Serialize>(&mut self, _data: T) -> Result<()> {
                Ok(())
            }

            fn is_json(&self) -> bool {
                true
            }
        }

        fn client(dir: &tempfile::TempDir, strategy: TimeStrategy) -> TaskClient {
            let account = AccountConfig {
                vdir: Some(VdirConfig {
                    home_dir: dir.path().join("home"),
                }),
                time: TimeConfig {
                    strategy: Some(strategy),
                    collection: Some("tasks".into()),
                    directory: Some(dir.path().join("time")),
                },
                ..Default::default()
            };
            TaskClient::new(Config::default(), account, Backend::Auto).unwrap()
        }

        fn import(dir: &tempfile::TempDir, strategy: TimeStrategy) -> Result<()> {
            let store = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/store-1.x");
            ImportCommand::try_parse_from(["import", "-l", "tasks", store])
                .unwrap()
                .execute(&mut Quiet, client(dir, strategy))
        }

        fn items(dir: &tempfile::TempDir, kind: IcalComponentKind) -> usize {
            let query = TaskItemQuery {
                kind: Some(kind),
                ..Default::default()
            };
            client(dir, TimeStrategy::Local)
                .list_items("tasks", query)
                .unwrap()
                .len()
        }

        #[test]
        fn an_import_writes_every_task_and_time_entry_once_locally_by_default() {
            let dir = tempfile::tempdir().unwrap();
            std::fs::create_dir_all(dir.path().join("home/tasks")).unwrap();

            import(&dir, TimeStrategy::Local).unwrap();

            assert_eq!(items(&dir, IcalComponentKind::VTodo), 4);
            let time = std::fs::read_dir(dir.path().join("time")).unwrap().count();
            assert_eq!(time, 2);

            // NOTE: a second run finds every task and every entry there.
            import(&dir, TimeStrategy::Local).unwrap();

            assert_eq!(items(&dir, IcalComponentKind::VTodo), 4);
            let time = std::fs::read_dir(dir.path().join("time")).unwrap().count();
            assert_eq!(time, 2);
        }

        #[test]
        fn an_import_under_the_journal_strategy_writes_vjournal_pairs() {
            let dir = tempfile::tempdir().unwrap();
            std::fs::create_dir_all(dir.path().join("home/tasks")).unwrap();

            import(&dir, TimeStrategy::Journal).unwrap();
            import(&dir, TimeStrategy::Journal).unwrap();

            assert_eq!(items(&dir, IcalComponentKind::VTodo), 4);
            assert_eq!(items(&dir, IcalComponentKind::VJournal), 4);
            assert!(!dir.path().join("time").exists());
        }
    }
}
