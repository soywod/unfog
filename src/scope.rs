//! # Scope
//!
//! The accounts a GTD view or a time command spans: every configured
//! account, or the one `-a/--account` names, each through the backend
//! `--backend` selects for it.
//!
//! The views are global (pimdir GUIDE.md §17): a task captured in one
//! account and clarified into another shows in the same Next. A task is
//! found across the whole scope by its handle or a prefix of its `UID`,
//! as within one tasklist.

use std::path::PathBuf;

use anyhow::{Result, bail};
use log::debug;
use pimalaya_cli::printer::Printer;
use pimalaya_config::toml::TomlConfig;

use crate::{
    backend::Backend,
    cli::load_config,
    config::Config,
    shared::{
        client::TaskClient,
        item::TaskItem,
        task::{Task, find::undashed},
    },
};

/// The accounts a command spans, each with its backend client.
pub struct Scope {
    /// The accounts, the default one first, then by name.
    pub accounts: Vec<ScopeAccount>,
}

/// One account of a [`Scope`].
pub struct ScopeAccount {
    /// The account's name in the configuration.
    pub name: String,
    /// The client of the backend selected for it.
    pub client: TaskClient,
}

/// A task found in a scope, with where it lives.
#[derive(Clone, Debug)]
pub struct Located {
    /// The index of its account in [`Scope::accounts`].
    pub account: usize,
    /// The tasklist holding it.
    pub tasklist: String,
    /// The task.
    pub task: Task,
}

impl Scope {
    /// Loads the configuration and opens every account `account_name`
    /// lets in: that one alone when given, else every account.
    ///
    /// An account carrying no backend `backend` allows is left out, unless
    /// it is the one named; a scope left with no account is refused.
    pub fn resolve(
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
        backend: Backend,
    ) -> Result<Self> {
        let config = load_config(printer, config_paths)?;
        Self::new(config, account_name, backend)
    }

    /// Opens the accounts of `config` `account_name` lets in.
    pub fn new(config: Config, account_name: Option<&str>, backend: Backend) -> Result<Self> {
        let mut names: Vec<(bool, String)> = match account_name {
            Some(name) => vec![(true, name.to_owned())],
            None => config
                .accounts
                .iter()
                .map(|(name, account)| (!account.default, name.clone()))
                .collect(),
        };
        names.sort();

        let mut accounts = Vec::with_capacity(names.len());

        for (_, name) in names {
            let mut config = config.clone();

            let Some((name, account_config)) = config.take_account(Some(&name))? else {
                bail!("Cannot find account `{name}`");
            };

            config.accounts.clear();

            match TaskClient::new(config, account_config, backend) {
                Ok(client) => accounts.push(ScopeAccount { name, client }),
                Err(err) if account_name.is_none() => {
                    debug!("leaving account `{name}` out: {err}");
                }
                Err(err) => return Err(err),
            }
        }

        if accounts.is_empty() {
            bail!("No account configured with a backend matching `{backend}`");
        }

        Ok(Self { accounts })
    }

    /// Whether the scope spans more than one account, which a listing
    /// then names.
    pub fn is_multi(&self) -> bool {
        self.accounts.len() > 1
    }

    /// The name of the account at `index`.
    pub fn name(&self, index: usize) -> &str {
        &self.accounts[index].name
    }

    /// The client of the account at `index`.
    pub fn client(&mut self, index: usize) -> &mut TaskClient {
        &mut self.accounts[index].client
    }

    /// The index of the account named `name`.
    pub fn index_of(&self, name: &str) -> Result<usize> {
        match self
            .accounts
            .iter()
            .position(|account| account.name == name)
        {
            Some(index) => Ok(index),
            None => bail!("Account `{name}` is not configured, or carries no matching backend"),
        }
    }

    /// Every task of every tasklist of the scope whose status and
    /// categories `keep` accepts.
    pub fn select(
        &mut self,
        keep: &dyn Fn(Option<&str>, &[String]) -> bool,
    ) -> Result<Vec<Located>> {
        let mut found = Vec::new();

        for (index, account) in self.accounts.iter_mut().enumerate() {
            for tasklist in account.client.list_tasklists()? {
                let tasks = account.client.select_tasks(&tasklist.id, keep)?;

                found.extend(tasks.into_iter().map(|task| Located {
                    account: index,
                    tasklist: tasklist.id.clone(),
                    task,
                }));
            }
        }

        Ok(found)
    }

    /// Every task of the scope.
    pub fn tasks(&mut self) -> Result<Vec<Located>> {
        self.select(&|_, _| true)
    }

    /// Finds the task `query` names across the scope: by the handle of its
    /// item, else by a prefix of its `UID`, dashes ignored.
    ///
    /// A prefix matching several tasks is refused naming them, never
    /// guessed.
    pub fn find(&mut self, query: &str) -> Result<Located> {
        let query = query.trim();

        if query.is_empty() {
            bail!("Cannot find a task from an empty id");
        }

        let tasks = self.tasks()?;

        let by_handle: Vec<&Located> = tasks
            .iter()
            .filter(|found| found.task.id == query)
            .collect();

        if let [found] = by_handle.as_slice() {
            return Ok((*found).clone());
        }

        let prefix = undashed(query);
        let mut matching: Vec<&Located> = match by_handle.is_empty() {
            true => tasks
                .iter()
                .filter(|found| undashed(&found.task.uid).starts_with(&prefix))
                .collect(),
            false => by_handle,
        };

        // NOTE: an item may hold several VTODOs sharing one UID (a
        // recurring task and its overrides), which are one task.
        matching.dedup_by(|a, b| {
            a.account == b.account && a.tasklist == b.tasklist && a.task.id == b.task.id
        });

        match matching.as_slice() {
            [] => bail!("Cannot find task `{query}`"),
            [found] => Ok((*found).clone()),
            several => {
                let names: Vec<String> = several
                    .iter()
                    .map(|found| {
                        format!(
                            "{} ({}/{})",
                            found.task.uid,
                            self.name(found.account),
                            found.tasklist
                        )
                    })
                    .collect();
                bail!(
                    "Task id `{query}` is ambiguous, it names {}; give more of it",
                    names.join(", ")
                )
            }
        }
    }

    /// The item holding a located task, read afresh so its entity tag
    /// guards the write that follows.
    pub fn item(&mut self, found: &Located) -> Result<TaskItem> {
        self.client(found.account)
            .get_item(&found.tasklist, &found.task.id)
    }
}
