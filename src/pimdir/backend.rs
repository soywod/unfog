//! # Pimdir backend
//!
//! The shared-API adapter over a pimdir store, projecting its items
//! through [`io_pimdir`]'s reader plus the blob store.
//!
//! An item whose body is not local still lists, carrying no bytes; only
//! a read of it reports "body not fetched", the cue to sync rather than
//! a data-loss error. Its kind answers from the typed summary the store
//! keeps beside the row (pimdir STORAGE Annex A).
//!
//! Writes append one action to the store's queue (pimdir SPEC 15.1):
//! the body reaches the blob tree first, then the row pinning it. The
//! owner, a sync, derives the summary from that body, applies and
//! pushes it, while the reader folds the pending queue over its reads
//! so a staged change shows before that.
//!
//! Collections come from the sync, so the collection verbs (create,
//! delete) are not served here: a cache does not invent collections its
//! source does not have.
//!
//! Ids are the store's public `seq`, stable across every collection an
//! item is filed in, never the internal link id.

use std::{io::Write, ops::ControlFlow, path::PathBuf};

use anyhow::{Result, anyhow, bail};
use ical::component::IcalComponentKind;
use io_pimdir::{
    capability::CALENDAR_SCHEDULING,
    client::{
        PimdirError, TIME_COLLECTION,
        reader::{PimdirCollection, PimdirItem},
    },
    codec::PimdirAction,
    object::PimdirObject,
    placement::PimdirFlags,
    summary::{
        PimdirSummary,
        calendar::{PimdirTaskSummary, derive},
    },
};
use log::warn;
use pimalaya_cli::{error::CodedError, printer::Printer};
use pimalaya_config::toml::TomlConfig;

use crate::{
    cli::load_config,
    config::PimdirConfig,
    error::ErrorCode,
    pimdir::{
        client::PimdirClient,
        status::{PimdirCalendarStatus, PimdirStatusOutput},
    },
    shared::{
        ical::holds_kind,
        item::{TaskItem, TaskItemQuery},
        task::Task,
        tasklist::Tasklist,
    },
};

/// The media type a pimdir collection carries to be a calendar.
const CALENDAR_KIND: &str = "text/calendar";

/// How many items to pull per keyset page when scanning a collection.
#[cfg(not(test))]
const SCAN_BATCH: usize = 500;

/// A batch small enough for a test store to span several.
#[cfg(test)]
const SCAN_BATCH: usize = 7;

/// The shared-API glue over a pimdir store.
pub struct PimdirBackend {
    client: PimdirClient,
}

impl PimdirBackend {
    /// Opens the store at the configured root.
    pub fn new(config: PimdirConfig) -> Result<Self> {
        Ok(Self {
            client: PimdirClient::new(config)?,
        })
    }

    /// Loads the configuration, picks the active account, then opens
    /// the store.
    ///
    /// Bails when the account carries no `[pimdir]` block.
    pub fn build(
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
    ) -> Result<Self> {
        let mut config = load_config(printer, config_paths)?;
        let (name, mut account_config) = config
            .take_account(account_name)?
            .ok_or_else(|| anyhow!("Cannot find account"))?;

        let pimdir_config = account_config
            .pimdir
            .take()
            .ok_or_else(|| anyhow!("pimdir configuration is missing for account `{name}`"))?;

        Self::new(pimdir_config)
    }

    /// Lists the calendar collections.
    ///
    /// Those declaring `text/calendar`, plus the kind-less ones a sync
    /// created before any consumer declared a kind.
    pub fn list_tasklists(&mut self) -> Result<Vec<Tasklist>> {
        let mut tasklists: Vec<Tasklist> = self
            .calendar_collections()?
            .into_iter()
            .map(|collection| Tasklist {
                name: if collection.name.is_empty() {
                    collection.id.clone()
                } else {
                    collection.name
                },
                id: collection.id,
                description: collection.description,
                color: collection.color,
                default: collection.role.as_deref() == Some("default"),
            })
            .collect();

        tasklists.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(tasklists)
    }

    /// Refuses to create a tasklist.
    ///
    /// Declaring a collection is an owner write (pimdir SPEC 8) and
    /// this backend is a producer, and a collection no sync knows about
    /// is one no sync would carry.
    pub fn create_tasklist(
        &mut self,
        _id: &str,
        _name: &str,
        _description: Option<&str>,
        _color: Option<&str>,
    ) -> Result<String> {
        bail!(unsupported("create"))
    }

    /// Refuses to delete a tasklist, for the same reason as
    /// [`create_tasklist`](Self::create_tasklist).
    pub fn delete_tasklist(&mut self, _id: &str) -> Result<()> {
        bail!(unsupported("delete"))
    }

    /// Lists a collection's items, reading each local body.
    ///
    /// An unhydrated item lists with empty contents, its kind answered
    /// from the stored summary. The collection streams through in keyset
    /// batches, one body held at a time, and the scan stops once the
    /// page is full. A body is read only to answer the kind or to be
    /// returned, never for an item the stored summary already rules out.
    pub fn list_items(&mut self, tasklist_id: &str, query: TaskItemQuery) -> Result<Vec<TaskItem>> {
        self.known_collection(tasklist_id)?;

        let mut page = Window::new(query.page, query.page_size);
        if page.is_closed() {
            return Ok(Vec::new());
        }

        let restaged = match query.kind {
            Some(_) => self.restaged()?,
            None => Vec::new(),
        };

        self.scan(tasklist_id, |stored| {
            if ruled_out(&stored, query.kind, &restaged) {
                return Ok(ControlFlow::Continue(()));
            }

            // NOTE: with no kind to answer, an item before the page needs
            // no body to be counted.
            if query.kind.is_none() && page.skips() {
                return Ok(ControlFlow::Continue(()));
            }

            let item = self.item_from(tasklist_id, &stored)?;

            if let Some(kind) = query.kind {
                if !is_kind(&item, &stored, kind) {
                    return Ok(ControlFlow::Continue(()));
                }

                if page.skips() {
                    return Ok(ControlFlow::Continue(()));
                }
            }

            Ok(page.keep(item))
        })?;

        Ok(page.into_items())
    }

    /// The tasks of a collection whose status and categories `keep`
    /// accepts, decided from the task summaries the store keeps.
    ///
    /// `task_summary.status` and `task_summary.categories` are the
    /// columns pimdir's guide reads its views from (GUIDE.md §17), so a
    /// task `keep` refuses is passed over unread. A task it keeps is
    /// projected from its body, or from its summary while the body is not
    /// local; one a pending `update` repoints, or that carries no summary
    /// yet, is decided from its body instead, the summary describing the
    /// old one.
    pub fn select_tasks(
        &mut self,
        tasklist_id: &str,
        keep: &dyn Fn(Option<&str>, &[String]) -> bool,
    ) -> Result<Vec<Task>> {
        self.known_collection(tasklist_id)?;

        let restaged = self.restaged()?;
        let mut tasks = Vec::new();

        self.scan(tasklist_id, |stored| {
            let summary = match &stored.summary {
                _ if restaged.contains(&stored.seq) => None,
                Some(PimdirSummary::Task(summary)) => Some(summary),
                Some(_) if stored.object.is_some() => return Ok(ControlFlow::Continue(())),
                _ => None,
            };

            if let Some(summary) = summary
                && !keep(summary.status.as_deref(), &summary.categories)
            {
                return Ok(ControlFlow::Continue(()));
            }

            let item = self.item_from(tasklist_id, &stored)?;

            if item.contents.is_empty() {
                if let Some(summary) = summary {
                    tasks.push(task_from_summary(stored.seq, summary));
                }
                return Ok(ControlFlow::Continue(()));
            }

            tasks.extend(
                Task::project(&item)
                    .into_iter()
                    .filter(|task| keep(task.status.as_deref(), &task.categories)),
            );

            Ok(ControlFlow::Continue(()))
        })?;

        Ok(tasks)
    }

    /// Reads one item's bytes from its content-addressed blob.
    ///
    /// Fails with a clear "body not fetched" when the item is not
    /// hydrated: a state to resolve with a sync, not a missing item.
    pub fn get_item(&mut self, tasklist_id: &str, item_id: &str) -> Result<TaskItem> {
        self.known_collection(tasklist_id)?;

        let stored = self.item(tasklist_id, item_id)?;

        let Some(hash) = stored.object else {
            return Err(CodedError::new(
                ErrorCode::BodyPending,
                format!(
                    "Item `{item_id}` in tasklist `{tasklist_id}` is not downloaded yet \
                     (body not fetched); run a sync to hydrate it"
                ),
            )
            .into());
        };

        let contents = self.client.blobs.get(&hash)?.ok_or_else(|| {
            anyhow!("Body blob missing for item `{item_id}` in tasklist `{tasklist_id}`")
        })?;

        Ok(TaskItem {
            id: stored.seq.to_string(),
            tasklist_id: tasklist_id.to_owned(),
            etag: Some(hash.0),
            contents,
        })
    }

    /// Stages a locally-authored item as an `add` the next sync pushes.
    ///
    /// Returns the item's link id, its `UID` under the store's own
    /// derivation (pimdir STORAGE Annex A.3): a queued create carries no
    /// public `seq` until the store's owner applies it, so there is no
    /// store-assigned id to report yet.
    pub fn create_item(&mut self, tasklist_id: &str, contents: Vec<u8>) -> Result<String> {
        self.known_collection(tasklist_id)?;

        let link_id = derive(&contents).link_id;
        let hash = self.client.reader.hash(&contents);

        let action = PimdirAction::Add {
            link_id: Some(link_id.clone()),
            flags: PimdirFlags::default(),
            object: Some(hash),
        };

        let object = self.stage_body(&contents)?;
        self.enqueue(tasklist_id, &action, Some(&object))?;

        Ok(link_id.0)
    }

    /// Stages a body replacement as an `update` the next sync pushes.
    ///
    /// `if_match` gates the staging on the item's version, the store's
    /// hash of its body with the pending queue folded in. The engine
    /// still three-way merges the update against the base body it
    /// recorded at sync time: the gate is the caller's, the merge the
    /// store's.
    pub fn update_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        contents: Vec<u8>,
        if_match: Option<&str>,
    ) -> Result<()> {
        self.known_collection(tasklist_id)?;

        let stored = self.item(tasklist_id, item_id)?;
        check_version(&stored, tasklist_id, item_id, if_match)?;

        let action = PimdirAction::Update {
            seq: stored.seq,
            object: self.client.reader.hash(&contents),
        };

        let object = self.stage_body(&contents)?;
        self.enqueue(tasklist_id, &action, Some(&object))
    }

    /// Stages a `remove` action: a tombstone, then a server-side delete.
    ///
    /// `if_match` gates it as it gates [`update_item`](Self::update_item).
    pub fn delete_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        if_match: Option<&str>,
    ) -> Result<()> {
        self.known_collection(tasklist_id)?;

        let stored = self.item(tasklist_id, item_id)?;
        check_version(&stored, tasklist_id, item_id, if_match)?;
        let seq = stored.seq;
        self.enqueue(tasklist_id, &PimdirAction::Remove { seq }, None)
    }

    /// Collects the accounts and per-calendar hydration state that
    /// `pimdir status` reports.
    pub fn status(&mut self) -> Result<PimdirStatusOutput> {
        let accounts = self.client.reader.list_accounts()?;
        let mut calendars = Vec::new();

        for collection in self.calendar_collections()? {
            let (mut total, mut hydrated) = (0, 0);
            self.scan(&collection.id, |item| {
                total += 1;
                hydrated += usize::from(item.object.is_some());
                Ok(ControlFlow::Continue(()))
            })?;
            let queued = self.client.reader.count_pending_creates(&collection.id)?;

            calendars.push(PimdirCalendarStatus {
                name: if collection.name.is_empty() {
                    collection.id.clone()
                } else {
                    collection.name
                },
                id: collection.id,
                total,
                hydrated,
                queued,
            });
        }

        calendars.sort_by(|a, b| a.name.cmp(&b.name));

        Ok(PimdirStatusOutput {
            account: self.client.account.clone(),
            accounts,
            calendars,
        })
    }

    /// The store's calendar collections.
    ///
    /// Narrowed to the configured account when the store groups several
    /// (pimdir SPEC 9.2).
    fn calendar_collections(&self) -> Result<Vec<PimdirCollection>> {
        let collections = match self.client.account.as_deref() {
            Some(account) => self
                .client
                .reader
                .list_collections_by_account(Some(account))?,
            None => self.client.reader.list_collections()?,
        };

        // NOTE: the local time collection holds time entries, never tasks,
        // and is the time strategy's to read (pimdir STORAGE §14.3).
        Ok(collections
            .into_iter()
            .filter(|collection| collection.kind.is_empty() || collection.kind == CALENDAR_KIND)
            .filter(|collection| collection.id != TIME_COLLECTION)
            .collect())
    }

    /// Fails unless `tasklist_id` names a calendar the store knows.
    ///
    /// The read seam answers an unknown collection with an empty page
    /// and the queue takes any name, so a typo in `-l` would read as an
    /// empty calendar and stage into one nothing ever applies.
    fn known_collection(&self, tasklist_id: &str) -> Result<()> {
        let mut ids: Vec<String> = self
            .calendar_collections()?
            .into_iter()
            .map(|collection| collection.id)
            .collect();

        if ids.iter().any(|id| id == tasklist_id) {
            return Ok(());
        }

        ids.sort();

        bail!(
            "Tasklist `{tasklist_id}` not found; this account holds: {}",
            ids.join(", "),
        )
    }

    /// Walks every live item of a collection by keyset paging, each
    /// with its summary joined, until `visit` breaks.
    ///
    /// In the order the store maintains for calendars, start ascending.
    /// One batch of rows is held at a time, never the collection.
    fn scan(
        &self,
        tasklist_id: &str,
        mut visit: impl FnMut(PimdirItem) -> Result<ControlFlow<()>>,
    ) -> Result<()> {
        let mut cursor: Option<(String, i64)> = None;

        loop {
            let batch = self.client.reader.list_summaries(
                tasklist_id,
                cursor.as_ref().map(|(key, seq)| (key.as_str(), *seq)),
                SCAN_BATCH,
            )?;
            let count = batch.len();

            if let Some(last) = batch.last() {
                cursor = Some((last.sort_key.clone(), last.seq));
            }

            for item in batch {
                if visit(item)?.is_break() {
                    return Ok(());
                }
            }

            if count < SCAN_BATCH {
                return Ok(());
            }
        }
    }

    /// The public ids of the items a pending `update` repoints, store
    /// wide.
    ///
    /// The reader folds the new body in, while the summary beside the
    /// row is still the one the owner derived from the old body, so it
    /// cannot rule such an item out.
    fn restaged(&self) -> Result<Vec<i64>> {
        Ok(self
            .client
            .reader
            .list_pending_actions()?
            .into_iter()
            .filter_map(|pending| match pending.action {
                PimdirAction::Update { seq, .. } => Some(seq),
                _ => None,
            })
            .collect())
    }

    /// The stored item behind a public item id, or a clear miss.
    ///
    /// Anything non-numeric is the internal link id or a mistyped
    /// value, and saying so beats a lookup that silently finds nothing.
    fn item(&self, tasklist_id: &str, item_id: &str) -> Result<PimdirItem> {
        let seq = item_id.parse::<i64>().map_err(|_| {
            anyhow!("Invalid pimdir item id `{item_id}`: expected the number a listing showed")
        })?;

        self.client
            .reader
            .get_item(tasklist_id, seq)?
            .ok_or_else(|| anyhow!("Item `{item_id}` not found in calendar `{tasklist_id}`"))
    }

    /// Projects a stored item onto the shared type, body when local.
    ///
    /// Empty contents mean two things and only one is ordinary: the
    /// sync has not hydrated the item, or its object's blob is gone,
    /// an inconsistent store, logged since the row renders alike.
    fn item_from(&self, tasklist_id: &str, stored: &PimdirItem) -> Result<TaskItem> {
        let contents = match &stored.object {
            Some(hash) => match self.client.blobs.get(hash)? {
                Some(contents) => contents,
                None => {
                    warn!(
                        "body blob missing for item `{}` in calendar `{tasklist_id}`, \
                         listing it without contents",
                        stored.seq
                    );
                    Vec::new()
                }
            },
            None => Vec::new(),
        };

        Ok(TaskItem {
            id: stored.seq.to_string(),
            tasklist_id: tasklist_id.to_owned(),
            etag: stored.object.as_ref().map(|hash| hash.0.clone()),
            contents,
        })
    }

    /// Writes a body into the blob tree, returning the object it indexes.
    ///
    /// Durable before anything references it (pimdir SPEC 14), so the
    /// queue row appended next pins a body that is already there.
    fn stage_body(&self, contents: &[u8]) -> Result<PimdirObject> {
        // NOTE: the hash is the store's, from `store_meta.hash_algo`,
        // never one this crate picks: a body named under another
        // algorithm is one no read ever finds.
        let hash = self.client.reader.hash(contents);
        let mut writer = self.client.blobs.writer()?;
        writer.write_all(contents)?;
        let size = writer.commit(&hash)?;

        Ok(PimdirObject {
            hash,
            size: size as usize,
        })
    }

    /// Appends one action to a collection's queue.
    ///
    /// The producer is opened for this write and dropped with it.
    fn enqueue(
        &mut self,
        tasklist_id: &str,
        action: &PimdirAction,
        object: Option<&PimdirObject>,
    ) -> Result<()> {
        let mut producer = self.client.producer()?;
        let partials = producer.check(tasklist_id, action).map_err(staging_error)?;
        producer
            .enqueue(tasklist_id, action, object)
            .map_err(staging_error)?;

        for partial in &partials {
            warn!("{partial}");
        }
        Ok(())
    }
}

/// What a write the store refuses to stage reads as, naming the way out
/// where there is one.
fn staging_error(err: PimdirError) -> anyhow::Error {
    match err {
        PimdirError::Unsupported(refusal) if refusal.capability == CALENDAR_SCHEDULING => {
            anyhow!(
                "Stage the pimdir action: {refusal}; to write it without notifying \
                 anyone, first mark its ORGANIZER and ATTENDEE with \
                 SCHEDULE-AGENT=NONE"
            )
        }
        err => anyhow!("Stage the pimdir action: {err}"),
    }
}

/// How the refusal of a write whose `--if-match` names a version the
/// item no longer has starts, a prefix callers may match on.
pub const PRECONDITION_FAILED: &str = "Precondition failed";

/// Fails unless `if_match` names the item's current version.
///
/// The version is the store's hash of the item's body, the pending
/// queue folded in: the `etag` a read or a listing reports. An item
/// whose body is not local has no version to match. Surrounding double
/// quotes are dropped, so an HTTP-quoted tag matches as well.
fn check_version(
    stored: &PimdirItem,
    tasklist_id: &str,
    item_id: &str,
    if_match: Option<&str>,
) -> Result<()> {
    let Some(expected) = if_match else {
        return Ok(());
    };
    let expected = expected.trim().trim_matches('"');
    let current = stored.object.as_ref().map(|hash| hash.0.as_str());

    if current == Some(expected) {
        return Ok(());
    }

    Err(CodedError::new(
        ErrorCode::PreconditionFailed,
        format!(
            "{PRECONDITION_FAILED}: item `{item_id}` in tasklist `{tasklist_id}` is at \
             version `{}`, not `{expected}`; read it again",
            current.unwrap_or("none (body not fetched)"),
        ),
    )
    .into())
}

/// Whether the stored summary proves a local item is not of `kind`, so a
/// listing need not read its body.
///
/// Only a negative answer is drawn, and only from what the format makes
/// true of every resource: one component type, filed in that type's
/// table (pimdir STORAGE Annex A.3). An item with no local body is
/// answered from its summary anyway, without a read, and one a pending
/// `update` repoints (`restaged`) carries a summary of its old body, so
/// neither is ruled out here.
fn ruled_out(stored: &PimdirItem, kind: Option<IcalComponentKind>, restaged: &[i64]) -> bool {
    let Some(kind) = kind else {
        return false;
    };

    if stored.object.is_none() || restaged.contains(&stored.seq) {
        return false;
    }

    let table = match &stored.summary {
        Some(PimdirSummary::Event(_)) => IcalComponentKind::VEvent,
        Some(PimdirSummary::Task(_)) => IcalComponentKind::VTodo,
        Some(PimdirSummary::Journal(_)) => IcalComponentKind::VJournal,
        _ => return false,
    };

    kind != table
}

/// The page a streamed listing keeps: 1-indexed, a `None` size keeping
/// every match, as the shared `paginate` cuts a whole list.
struct Window<T> {
    /// Matches still to pass over before the page starts.
    skip: usize,
    /// The page size, `None` for no cap.
    take: Option<usize>,
    items: Vec<T>,
}

impl<T> Window<T> {
    fn new(page: Option<u32>, page_size: Option<u32>) -> Self {
        let (skip, take) = match page_size {
            None => (0, None),
            Some(size) => {
                let page = page.unwrap_or(1).max(1);
                let skip = ((page - 1) as usize).saturating_mul(size as usize);
                (skip, Some(size as usize))
            }
        };

        Self {
            skip,
            take,
            items: Vec::new(),
        }
    }

    /// Whether the page holds nothing whatever the collection holds.
    fn is_closed(&self) -> bool {
        self.take == Some(0)
    }

    /// Passes over one match when the page has not started yet.
    fn skips(&mut self) -> bool {
        match self.skip {
            0 => false,
            _ => {
                self.skip -= 1;
                true
            }
        }
    }

    /// Keeps one match, breaking once the page is full.
    fn keep(&mut self, item: T) -> ControlFlow<()> {
        self.items.push(item);

        match self.take {
            Some(take) if self.items.len() >= take => ControlFlow::Break(()),
            _ => ControlFlow::Continue(()),
        }
    }

    fn into_items(self) -> Vec<T> {
        self.items
    }
}

/// Whether a stored item is of `kind`.
///
/// A body the sync has not downloaded still answers: the summary table
/// the store filed it in names the component a reader renders the
/// resource as (pimdir STORAGE Annex A.3 to A.5).
fn is_kind(item: &TaskItem, stored: &PimdirItem, kind: IcalComponentKind) -> bool {
    if !item.contents.is_empty() {
        return holds_kind(&item.contents, kind);
    }

    match &stored.summary {
        Some(PimdirSummary::Event(_)) => kind == IcalComponentKind::VEvent,
        Some(PimdirSummary::Task(_)) => kind == IcalComponentKind::VTodo,
        Some(PimdirSummary::Journal(_)) => kind == IcalComponentKind::VJournal,
        _ => false,
    }
}

/// A task projected from the summary the store keeps of it, for a body
/// the sync has not downloaded yet.
fn task_from_summary(seq: i64, summary: &PimdirTaskSummary) -> Task {
    Task {
        id: seq.to_string(),
        uid: summary.uid.clone().unwrap_or_default(),
        summary: summary.summary.clone(),
        start: summary.dtstart.as_ref().map(|time| time.value.clone()),
        due: summary.due.as_ref().map(|time| time.value.clone()),
        status: summary.status.clone(),
        completed: summary.completed.clone(),
        priority: summary.priority,
        percent_complete: summary.percent,
        categories: summary.categories.clone(),
        parent: summary.parent.clone(),
        ..Task::default()
    }
}

/// The message a collection verb refuses with, naming the way out.
fn unsupported(verb: &str) -> String {
    format!(
        "pimdir cannot {verb} a tasklist: the store is an offline cache, and its collections \
         come from the sync engine that fills it. Run the operation against the account the \
         store syncs, then sync again."
    )
}

#[cfg(test)]
mod tests {
    use io_pimdir::{
        placement::{PimdirLevel, PimdirLinkId},
        summary::calendar::PimdirTaskSummary,
    };
    use pimalaya_cli::error::code_of;

    use crate::{
        config::PimdirConfig,
        error::ErrorCode,
        pimdir::backend::{
            CALENDAR_KIND, PRECONDITION_FAILED, PimdirBackend, is_kind, ruled_out, unsupported,
        },
        shared::item::{TaskItem, TaskItemQuery},
    };
    use ical::component::IcalComponentKind;
    use io_pimdir::{client::reader::PimdirItem, placement::PimdirFlags, summary::PimdirSummary};

    /// The tasklist the server names the default carries the mark, read
    /// from the role the sync engine recorded (pimdir STORAGE 14).
    #[test]
    fn the_default_tasklist_is_the_one_its_store_marks() {
        let dir = tempfile::tempdir().unwrap();
        {
            let store = io_pimdir::client::PimdirStore::open(dir.path())
                .unwrap()
                .for_account("work");
            for id in ["caldav/default", "caldav/team"] {
                store.ensure_collection(id, CALENDAR_KIND).unwrap();
            }
            store
                .set_collection_role("caldav/default", Some("default"))
                .unwrap();
        }

        let mut backend = PimdirBackend::new(PimdirConfig {
            root: dir.path().to_path_buf(),
            account: None,
        })
        .unwrap();
        let defaults: Vec<(String, bool)> = backend
            .list_tasklists()
            .unwrap()
            .into_iter()
            .map(|tasklist| (tasklist.id, tasklist.default))
            .collect();
        assert_eq!(
            defaults,
            [
                ("caldav/default".into(), true),
                ("caldav/team".into(), false)
            ]
        );
    }

    #[test]
    fn the_collection_refusal_points_at_the_sync() {
        let message = unsupported("create");
        assert!(message.contains("offline cache"));
        assert!(message.contains("sync"));
    }

    #[test]
    fn an_undownloaded_item_answers_its_kind_from_its_summary() {
        let task = stored(PimdirSummary::Task(PimdirTaskSummary::default()));
        let bare = PimdirItem {
            summary: None,
            ..task.clone()
        };

        assert!(is_kind(&undownloaded(), &task, IcalComponentKind::VTodo));
        assert!(!is_kind(&undownloaded(), &task, IcalComponentKind::VEvent));
        assert!(!is_kind(&undownloaded(), &bare, IcalComponentKind::VTodo));
    }

    #[test]
    fn a_local_item_of_another_kind_is_ruled_out_unread() {
        let mut task = stored(PimdirSummary::Task(PimdirTaskSummary::default()));
        task.object = Some(io_pimdir::object::PimdirHash("x".into()));

        assert!(ruled_out(&task, Some(IcalComponentKind::VEvent), &[]));
        assert!(!ruled_out(&task, Some(IcalComponentKind::VTodo), &[]));
        assert!(!ruled_out(&task, None, &[]));

        // NOTE: a pending update repoints the body, so the summary beside
        // the row may describe another kind.
        assert!(!ruled_out(&task, Some(IcalComponentKind::VEvent), &[1]));
    }

    /// The shared view of an item the sync has not hydrated.
    fn undownloaded() -> TaskItem {
        TaskItem {
            id: "1".into(),
            tasklist_id: "personal".into(),
            etag: None,
            contents: Vec::new(),
        }
    }

    /// A stored row at the `Meta` tier carrying `summary`.
    fn stored(summary: PimdirSummary) -> PimdirItem {
        PimdirItem {
            seq: 1,
            link_id: PimdirLinkId("task@example.org".into()),
            flags: PimdirFlags::default(),
            sort_key: String::new(),
            object: None,
            level: PimdirLevel::Meta,
            summary: Some(summary),
            retention: None,
        }
    }

    /// A task alone in its resource.
    const TASK: &str = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//t//t//EN\r\n\
        BEGIN:VTODO\r\nUID:task@example.org\r\nDTSTAMP:20261001T080000Z\r\n\
        SUMMARY:Focus\r\nEND:VTODO\r\nEND:VCALENDAR\r\n";

    /// A store holding one collection, `cal`, carrying `bodies` as items a
    /// sync applied, and the backend over it with their public ids.
    fn store(bodies: &[&str]) -> (tempfile::TempDir, PimdirBackend, Vec<String>) {
        let dir = tempfile::tempdir().unwrap();
        let store = io_pimdir::client::PimdirStore::open(dir.path()).unwrap();
        store.ensure_collection("cal", CALENDAR_KIND).unwrap();
        drop(store);

        let config = || PimdirConfig {
            root: dir.path().to_path_buf(),
            account: None,
        };

        let mut backend = PimdirBackend::new(config()).unwrap();
        for body in bodies {
            backend
                .create_item("cal", body.as_bytes().to_vec())
                .unwrap();
        }

        let mut owner = io_pimdir::client::PimdirStore::open(dir.path())
            .unwrap()
            .for_source("caldav");
        owner.drain().unwrap();
        drop(owner);

        let mut backend = PimdirBackend::new(config()).unwrap();
        let ids = backend
            .list_items("cal", TaskItemQuery::default())
            .unwrap()
            .into_iter()
            .map(|item| item.id)
            .collect();

        (dir, backend, ids)
    }

    /// The version a listing and a read report is the store's hash of the
    /// body, and a staged update moves it at once.
    #[test]
    fn the_version_is_the_body_hash_and_follows_a_staged_update() {
        let (_dir, mut backend, ids) = store(&[TASK]);

        let read = backend.get_item("cal", &ids[0]).unwrap();
        let listed = backend.list_items("cal", TaskItemQuery::default()).unwrap();
        let etag = read.etag.clone().unwrap();
        assert_eq!(listed[0].etag.as_deref(), Some(etag.as_str()));
        assert_eq!(etag, backend.client.reader.hash(TASK.as_bytes()).0);

        let edited = TASK.replace("SUMMARY:", "SUMMARY:Edited ");
        backend
            .update_item("cal", &ids[0], edited.clone().into_bytes(), Some(&etag))
            .unwrap();

        let moved = backend.get_item("cal", &ids[0]).unwrap().etag.unwrap();
        assert_ne!(moved, etag);
        assert_eq!(moved, backend.client.reader.hash(edited.as_bytes()).0);
    }

    #[test]
    fn a_stale_if_match_refuses_the_update_and_the_delete() {
        let (_dir, mut backend, ids) = store(&[TASK]);
        let pending = |backend: &PimdirBackend| {
            backend
                .client
                .producer()
                .unwrap()
                .pending_actions("cal")
                .unwrap()
                .len()
        };

        let update = backend.update_item("cal", &ids[0], TASK.into(), Some("stale"));
        let err = update.unwrap_err();
        assert_eq!(code_of(&err), Some(ErrorCode::PreconditionFailed));
        assert!(err.to_string().starts_with(PRECONDITION_FAILED), "{err}");

        let delete = backend.delete_item("cal", &ids[0], Some("stale"));
        assert!(
            delete
                .unwrap_err()
                .to_string()
                .starts_with(PRECONDITION_FAILED)
        );
        assert_eq!(pending(&backend), 0);

        let etag = backend.get_item("cal", &ids[0]).unwrap().etag.unwrap();
        backend
            .delete_item("cal", &ids[0], Some(&format!("\"{etag}\"")))
            .unwrap();
        assert_eq!(pending(&backend), 1);
        assert!(backend.get_item("cal", &ids[0]).is_err());
    }

    #[test]
    fn a_task_listing_keeps_the_vtodos_alone() {
        let event = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//t//t//EN\r\n\
            BEGIN:VEVENT\r\nUID:event@example.org\r\nDTSTAMP:20261001T080000Z\r\n\
            DTSTART:20261005T090000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";
        let (_dir, mut backend, ids) = store(&[TASK, event]);
        assert_eq!(ids.len(), 2);

        let query = TaskItemQuery {
            kind: Some(IcalComponentKind::VTodo),
            ..Default::default()
        };
        let items = backend.list_items("cal", query).unwrap();

        assert_eq!(items.len(), 1);
        assert_eq!(items[0].contents, TASK.as_bytes());
    }
}
