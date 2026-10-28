//! # Account check
//!
//! The `account check` command, and the per-backend test the wizard
//! reuses on the account it has just built.

use std::{fmt, path::PathBuf};

use anyhow::{Result, anyhow, bail};
use clap::Parser;
use pimalaya_cli::printer::Printer;
#[cfg(feature = "caldav")]
use pimalaya_config::secret::SecretResolver;
use pimalaya_config::toml::TomlConfig;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    backend::Backend,
    config::{AccountConfig, Config, NO_CONFIG_HINT},
};

/// Validate the account configuration.
///
/// Loads the configuration, picks the active account, then exercises
/// every backend the `--backend` flag allows: a local backend is
/// checked against the filesystem, CalDAV by opening the connection and
/// resolving the calendar home-set.
///
/// JSON output: `{"account", "backends": [{"backend", "ok", "error"}]}`.
#[derive(Debug, Parser)]
pub struct AccountCheckCommand;

impl AccountCheckCommand {
    pub fn execute(
        self,
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
        backend: Backend,
    ) -> Result<()> {
        let mut config = match Config::from_paths_or_default(config_paths)? {
            Some(config) => config,
            None => bail!("No configuration found, {NO_CONFIG_HINT}"),
        };

        let (name, account_config) = config
            .take_account(account_name)?
            .ok_or_else(|| anyhow!("Cannot find account"))?;

        let backends = check_account(&account_config, backend);

        if backends.is_empty() {
            bail!("No backend matching `{backend}` is configured for this account");
        }

        printer.out(AccountCheckOutput {
            account: name,
            backends,
        })
    }
}

/// Exercises every backend of `account_config` that `backend` allows,
/// one report row each.
///
/// Shared with the wizard, which tests the account it just built before
/// printing it.
///
/// The credentials go through one resolver for the whole account, so an
/// account whose blocks name the same command twice unlocks a
/// `pass` or `gpg` entry once rather than once per backend. It is
/// dropped with the check, holding plaintext while it lives.
pub fn check_account(
    #[allow(unused_variables)] account_config: &AccountConfig,
    #[allow(unused_variables)] backend: Backend,
) -> Vec<BackendCheck> {
    let mut checks = Vec::new();

    #[cfg(feature = "caldav")]
    let mut resolver = SecretResolver::new();

    #[cfg(feature = "vdir")]
    if backend.allows_vdir()
        && let Some(config) = account_config.vdir.clone()
    {
        checks.push(BackendCheck::from("vdir", check_vdir(config)));
    }

    #[cfg(feature = "pimdir")]
    if backend.allows_pimdir()
        && let Some(config) = account_config.pimdir.clone()
    {
        checks.push(BackendCheck::from("pimdir", check_pimdir(config)));
    }

    #[cfg(feature = "caldav")]
    if backend.allows_caldav()
        && let Some(config) = account_config.caldav.clone()
    {
        checks.push(BackendCheck::from(
            "caldav",
            check_caldav(config, &mut resolver),
        ));
    }

    checks
}

/// Whether every checked backend answered.
///
/// Only the wizard asks, so it compiles with the backends the wizard
/// configures.
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
pub fn all_ok(checks: &[BackendCheck]) -> bool {
    !checks.is_empty() && checks.iter().all(|check| check.ok)
}

/// The vdir home has to be a directory that exists: everything else is
/// a per-collection concern a command reports on its own.
///
/// The path arrives shell-expanded, `VdirConfig::home_dir` doing it at
/// deserialize.
#[cfg(feature = "vdir")]
fn check_vdir(config: crate::config::VdirConfig) -> Result<()> {
    if !config.home_dir.is_dir() {
        bail!(
            "vdir home `{}` does not exist or is not a directory",
            config.home_dir.display()
        );
    }

    Ok(())
}

/// Opening the store is the check: it creates the index when absent and
/// fails loudly when the path is not a usable store.
#[cfg(feature = "pimdir")]
fn check_pimdir(config: crate::config::PimdirConfig) -> Result<()> {
    crate::pimdir::client::PimdirClient::new(config)?;
    Ok(())
}

/// Connecting and resolving the calendar home-set exercises DNS,
/// TLS, authentication and the discovery chain in one go.
#[cfg(feature = "caldav")]
fn check_caldav(config: crate::config::CaldavConfig, resolver: &mut SecretResolver) -> Result<()> {
    crate::caldav::client::connect(&config, resolver)?;
    Ok(())
}

/// What `account check` reports.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct AccountCheckOutput {
    /// The account that was checked.
    pub account: String,
    /// One row per checked backend.
    pub backends: Vec<BackendCheck>,
}

/// One backend's verdict.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct BackendCheck {
    /// The backend name, as `--backend` spells it.
    pub backend: &'static str,
    /// Whether the backend answered.
    pub ok: bool,
    /// Why it did not, when it did not.
    pub error: Option<String>,
}

impl BackendCheck {
    fn from(backend: &'static str, result: Result<()>) -> Self {
        match result {
            Ok(()) => Self {
                backend,
                ok: true,
                error: None,
            },
            Err(err) => Self {
                backend,
                ok: false,
                error: Some(format!("{err:#}")),
            },
        }
    }
}

impl fmt::Display for AccountCheckOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Account: {}", self.account)?;

        for check in &self.backends {
            match &check.error {
                None => writeln!(f, "  {}: OK", check.backend)?,
                Some(err) => writeln!(f, "  {}: FAIL ({err})", check.backend)?,
            }
        }

        Ok(())
    }
}
