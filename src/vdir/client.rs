//! # Vdir client
//!
//! unfog's wrapper around [`io_vdir`]'s std client, bundling the
//! merged runtime [`Account`] alongside it for the protocol-specific
//! subcommands.

use std::{
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use anyhow::{Result, anyhow};
use io_vdir::{client::VdirClient as Inner, path::VdirPath};
use pimalaya_cli::printer::Printer;
use pimalaya_config::toml::TomlConfig;

use crate::{account::context::Account, cli::load_config, config::VdirConfig};

/// A vdir client rooted at the configured home directory.
pub struct VdirClient {
    inner: Inner,
    /// The merged account the commands read their rendering from.
    pub account: Account,
}

impl VdirClient {
    /// Builds the client rooted at the configured home directory.
    ///
    /// The path arrives shell-expanded, [`VdirConfig::home_dir`] doing
    /// it at deserialize.
    pub fn new(config: VdirConfig, account: Account) -> Self {
        let root = VdirPath::new(config.home_dir.to_string_lossy().into_owned());

        Self {
            inner: Inner::new(root),
            account,
        }
    }
}

impl Deref for VdirClient {
    type Target = Inner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for VdirClient {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// Loads the configuration, picks the active account, then opens it.
///
/// Bails when the account carries no `[vdir]` block.
pub fn build_vdir_client(
    printer: &mut impl Printer,
    config_paths: &[PathBuf],
    account_name: Option<&str>,
) -> Result<VdirClient> {
    let mut config = load_config(printer, config_paths)?;
    let (name, mut account_config) = config
        .take_account(account_name)?
        .ok_or_else(|| anyhow!("Cannot find account"))?;

    let vdir_config = account_config
        .vdir
        .take()
        .ok_or_else(|| anyhow!("vdir configuration is missing for account `{name}`"))?;

    let account = Account::from(config).merge(Account::from(account_config));

    Ok(VdirClient::new(vdir_config, account))
}
