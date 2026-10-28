//! # Task client
//!
//! The cross-protocol dispatcher the shared commands run on.
//!
//! One variant per compiled-in backend, a value always holding exactly one,
//! picked from the account configuration by the `--backend` flag. Each method
//! dispatches to the active backend, whose glue lives in that protocol
//! module's backend submodule.
//!
//! The surface is a strict least-common-denominator: an operation only
//! appears here when every backend can serve it, and anything narrower
//! belongs to a protocol-specific subcommand.

use std::path::PathBuf;

#[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
use std::convert::Infallible;

use anyhow::{Result, bail};
use ical::component::IcalComponentKind;
use log::debug;
use pimalaya_cli::printer::Printer;

use crate::{
    account::context::Account,
    backend::Backend,
    cli::resolve_account,
    config::{AccountConfig, Config},
    shared::{
        item::{TaskItem, TaskItemQuery},
        task::Task,
        tasklist::Tasklist,
    },
};

/// The active backend bundled with the merged runtime [`Account`].
///
/// The connection is opened on the first call that needs it and can be
/// dropped again with [`disconnect`](Self::disconnect). A command running
/// a composer holds none open while the editor is up: a server closes an
/// idle connection, and the write landing after a long edit would read
/// the end of a socket nobody is on the other end of any more.
pub struct TaskClient {
    /// What it takes to open the backend, kept so it can be reopened.
    config: BackendConfig,
    /// The open backend, `None` until something needs it.
    inner: Option<BackendClient>,
    /// The account the command runs against, config already merged.
    pub account: Account,
}

/// The configuration of the backend a [`TaskClient`] speaks to.
enum BackendConfig {
    #[cfg(feature = "vdir")]
    Vdir(crate::config::VdirConfig),
    #[cfg(feature = "caldav")]
    Caldav(Box<crate::config::CaldavConfig>),
    #[cfg(feature = "pimdir")]
    Pimdir(crate::config::PimdirConfig),
    /// Stands in when no backend is compiled in: never built, so every
    /// match over the backends stays exhaustive.
    #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
    Unused(Infallible),
}

/// Exactly one of the compiled-in per-backend glue clients.
enum BackendClient {
    #[cfg(feature = "vdir")]
    Vdir(crate::vdir::backend::VdirBackend),
    #[cfg(feature = "caldav")]
    Caldav(Box<crate::caldav::backend::CaldavBackend>),
    #[cfg(feature = "pimdir")]
    Pimdir(Box<crate::pimdir::backend::PimdirBackend>),
    /// Stands in when no backend is compiled in: never built, so every
    /// match over the backends stays exhaustive.
    #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
    Unused(Infallible),
}

impl TaskClient {
    /// Selects the backend from the account configuration.
    ///
    /// The first configured backend `backend` allows wins, in unfog's
    /// priority order: vdir, pimdir, CalDAV. That order prefers a local
    /// store to a network round-trip. Nothing is connected here: the
    /// first call that needs the network opens it.
    pub fn new(
        config: Config,
        #[allow(unused_mut)] mut account_config: AccountConfig,
        #[allow(unused_variables)] backend: Backend,
    ) -> Result<Self> {
        #[allow(unused_mut)]
        let mut selected: Option<BackendConfig> = None;

        #[cfg(feature = "vdir")]
        if selected.is_none()
            && backend.allows_vdir()
            && let Some(vdir_config) = account_config.vdir.take()
        {
            selected = Some(BackendConfig::Vdir(vdir_config));
        }

        #[cfg(feature = "pimdir")]
        if selected.is_none()
            && backend.allows_pimdir()
            && let Some(pimdir_config) = account_config.pimdir.take()
        {
            selected = Some(BackendConfig::Pimdir(pimdir_config));
        }

        #[cfg(feature = "caldav")]
        if selected.is_none()
            && backend.allows_caldav()
            && let Some(caldav_config) = account_config.caldav.take()
        {
            selected = Some(BackendConfig::Caldav(Box::new(caldav_config)));
        }

        let Some(config_) = selected else {
            bail!("No backend matching `{backend}` is configured for this account");
        };

        let account = Account::from(config).merge(Account::from(account_config));

        Ok(Self {
            config: config_,
            inner: None,
            account,
        })
    }

    /// Resolves the account a shared command runs against, and selects
    /// the backend serving it.
    pub fn resolve(
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
        backend: Backend,
    ) -> Result<Self> {
        let (config, account_config) = resolve_account(printer, config_paths, account_name)?;

        Self::new(config, account_config, backend)
    }

    /// Drops the connection, so the next call opens a fresh one.
    ///
    /// Called before a composer runs: an editor session lasts minutes,
    /// and a server that closed the idle connection meanwhile would fail
    /// the write that comes after it.
    pub fn disconnect(&mut self) {
        if self.inner.take().is_some() {
            debug!("closing the backend connection");
        }
    }

    /// The open backend, opening it when it is not.
    fn open(&mut self) -> Result<&mut BackendClient> {
        if self.inner.is_none() {
            debug!("opening the backend connection");
            self.inner = Some(self.config.open()?);
        }

        Ok(self.inner.as_mut().expect("just opened"))
    }

    /// Lists every tasklist available to the active account.
    pub fn list_tasklists(&mut self) -> Result<Vec<Tasklist>> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.list_tasklists(),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.list_tasklists(),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.list_tasklists(),
        }
    }

    /// Creates a tasklist under `id`, with a name and optional decorations.
    ///
    /// Returns the identifier the backend actually assigned.
    pub fn create_tasklist(
        &mut self,
        id: &str,
        name: &str,
        description: Option<&str>,
        color: Option<&str>,
    ) -> Result<String> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.create_tasklist(id, name, description, color),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.create_tasklist(id, name, description, color),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.create_tasklist(id, name, description, color),
        }
    }

    /// Deletes the tasklist `id` and every item it contains.
    pub fn delete_tasklist(&mut self, id: &str) -> Result<()> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.delete_tasklist(id),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.delete_tasklist(id),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.delete_tasklist(id),
        }
    }

    /// Lists the items of `tasklist_id`, narrowed by `query`.
    ///
    /// A backend SHALL narrow by kind before it paginates, so a page of
    /// tasks holds tasks.
    pub fn list_items(&mut self, tasklist_id: &str, query: TaskItemQuery) -> Result<Vec<TaskItem>> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.list_items(tasklist_id, query),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.list_items(tasklist_id, query),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.list_items(tasklist_id, query),
        }
    }

    /// The tasks of `tasklist_id` whose status and categories `keep`
    /// accepts, projected.
    ///
    /// CalDAV and vdir list the VTODOs and project them; pimdir decides
    /// from the task summaries the store keeps (`task_summary.status` and
    /// `categories`, the columns pimdir's guide queries), reading a body
    /// only for a task it keeps.
    pub fn select_tasks(
        &mut self,
        tasklist_id: &str,
        keep: &dyn Fn(Option<&str>, &[String]) -> bool,
    ) -> Result<Vec<Task>> {
        // NOTE: the one variant of a pimdir-only build.
        #[cfg(feature = "pimdir")]
        #[allow(irrefutable_let_patterns)]
        if let BackendClient::Pimdir(client) = self.open()? {
            return client.select_tasks(tasklist_id, keep);
        }

        let query = TaskItemQuery {
            kind: Some(IcalComponentKind::VTodo),
            ..Default::default()
        };

        Ok(self
            .list_items(tasklist_id, query)?
            .iter()
            .flat_map(Task::project)
            .filter(|task| keep(task.status.as_deref(), &task.categories))
            .collect())
    }

    /// The root of the pimdir store this client reads, `None` on any
    /// other backend.
    pub fn pimdir_root(&self) -> Option<PathBuf> {
        match &self.config {
            #[cfg(feature = "pimdir")]
            BackendConfig::Pimdir(config) => Some(config.root.clone()),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }

    /// Fetches the item `item_id` from `tasklist_id`.
    pub fn get_item(&mut self, tasklist_id: &str, item_id: &str) -> Result<TaskItem> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.get_item(tasklist_id, item_id),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.get_item(tasklist_id, item_id),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.get_item(tasklist_id, item_id),
        }
    }

    /// Stores raw iCalendar bytes as a new item, returning the id assigned.
    pub fn create_item(&mut self, tasklist_id: &str, contents: Vec<u8>) -> Result<String> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.create_item(tasklist_id, contents),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.create_item(tasklist_id, contents),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.create_item(tasklist_id, contents),
        }
    }

    /// Replaces the contents of `item_id` inside `tasklist_id`.
    ///
    /// `if_match` is the entity tag to gate the write on, `None` overwriting
    /// unconditionally.
    pub fn update_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        contents: Vec<u8>,
        if_match: Option<&str>,
    ) -> Result<()> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => {
                client.update_item(tasklist_id, item_id, contents, if_match)
            }
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => {
                client.update_item(tasklist_id, item_id, contents, if_match)
            }
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => {
                client.update_item(tasklist_id, item_id, contents, if_match)
            }
        }
    }

    /// Deletes `item_id` from `tasklist_id`, gated on `if_match` as
    /// [`update_item`](Self::update_item) is.
    pub fn delete_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        if_match: Option<&str>,
    ) -> Result<()> {
        match self.open()? {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            BackendClient::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            BackendClient::Vdir(client) => client.delete_item(tasklist_id, item_id, if_match),
            #[cfg(feature = "caldav")]
            BackendClient::Caldav(client) => client.delete_item(tasklist_id, item_id, if_match),
            #[cfg(feature = "pimdir")]
            BackendClient::Pimdir(client) => client.delete_item(tasklist_id, item_id, if_match),
        }
    }
}

impl BackendConfig {
    /// Opens the backend this configuration describes.
    fn open(&self) -> Result<BackendClient> {
        match self {
            #[cfg(not(any(feature = "vdir", feature = "caldav", feature = "pimdir")))]
            Self::Unused(never) => match *never {},
            #[cfg(feature = "vdir")]
            Self::Vdir(config) => {
                use crate::vdir::backend::VdirBackend;
                Ok(BackendClient::Vdir(VdirBackend::new(config.clone())))
            }
            #[cfg(feature = "pimdir")]
            Self::Pimdir(config) => {
                use crate::pimdir::backend::PimdirBackend;
                let client = PimdirBackend::new(config.clone())?;
                Ok(BackendClient::Pimdir(Box::new(client)))
            }
            #[cfg(feature = "caldav")]
            Self::Caldav(config) => {
                use crate::caldav::backend::CaldavBackend;
                let client = CaldavBackend::new(config.as_ref().clone())?;
                Ok(BackendClient::Caldav(Box::new(client)))
            }
        }
    }
}

/// 1-indexed pagination over an in-memory list.
///
/// `page_size = None` returns the whole slice, while a size of zero or a page
/// past the end returns nothing.
#[cfg(any(feature = "vdir", feature = "caldav", test))]
pub fn paginate<T>(items: Vec<T>, page: Option<u32>, page_size: Option<u32>) -> Vec<T> {
    let Some(size) = page_size else {
        return items;
    };

    if size == 0 {
        return Vec::new();
    }

    let page = page.unwrap_or(1).max(1);
    let skip = ((page - 1) as usize).saturating_mul(size as usize);

    if skip >= items.len() {
        return Vec::new();
    }

    items.into_iter().skip(skip).take(size as usize).collect()
}

#[cfg(test)]
mod tests {
    use crate::shared::client::paginate;

    #[test]
    fn pagination_windows_the_list_and_clamps_past_the_end() {
        let items = || vec![1, 2, 3, 4, 5];

        assert_eq!(paginate(items(), None, None), vec![1, 2, 3, 4, 5]);
        assert_eq!(paginate(items(), Some(1), Some(2)), vec![1, 2]);
        assert_eq!(paginate(items(), Some(3), Some(2)), vec![5]);
        assert!(paginate(items(), Some(9), Some(2)).is_empty());
        assert!(paginate(items(), None, Some(0)).is_empty());

        // NOTE: a page of zero is clamped to the first page rather than
        // underflowing the skip.
        assert_eq!(paginate(items(), Some(0), Some(2)), vec![1, 2]);
    }
}
