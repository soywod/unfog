//! # CLI
//!
//! The command tree unfog serves and its single dispatch point,
//! plus the configuration loading every command starts from.

use std::{
    io::{IsTerminal, stdin},
    path::{Path, PathBuf},
};

use anyhow::{Result, bail};
use clap::{CommandFactory, Parser, Subcommand};
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
use pimalaya_cli::prompt;
use pimalaya_cli::{
    clap::{
        args::{AccountFlag, JsonFlag, LogFlags},
        commands::{CompletionCommand, JsonSchemaCommand, ManualCommand},
        parsers::path_parser,
    },
    long_version,
    printer::Printer,
};
use pimalaya_config::toml::TomlConfig;

#[cfg(feature = "caldav")]
use crate::caldav::{cli::CaldavCommand, client::build_caldav_client};
#[cfg(feature = "pimdir")]
use crate::pimdir::{backend::PimdirBackend, cli::PimdirCommand};
#[cfg(feature = "vdir")]
use crate::vdir::{cli::VdirCommand, client::build_vdir_client};
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
use crate::wizard::{self, configure::ConfigureCommand};
use crate::{
    account::cli::AccountCommand,
    backend::Backend,
    config::{AccountConfig, Config, NO_CONFIG_HINT},
    gtd::cli::GtdCommand,
    import::ImportCommand,
    json_schema,
    scope::Scope,
    shared::{client::TaskClient, task::cli::TaskCommand, tasklist::cli::TasklistCommand},
    time::{migrate::TimeMigrateCommand, worktime::WorktimeCommand},
};

#[derive(Parser, Debug)]
#[command(name = env!("CARGO_PKG_NAME"))]
#[command(author, version, about)]
#[command(long_version = long_version!())]
#[command(propagate_version = true, infer_subcommands = true)]
pub struct UnfogCli {
    /// The command to run. Without one, the configuration wizard runs.
    #[command(subcommand)]
    pub command: Option<UnfogCommand>,

    /// Override the default configuration file path.
    ///
    /// Paths are shell-expanded then canonicalized. Several can be
    /// passed at once, `:`-delimited like `$PATH`, and are deep-merged
    /// left to right: that is how a public configuration and a private
    /// one stay separate files.
    #[arg(short, long = "config", global = true, env = "UNFOG_CONFIG")]
    #[arg(value_name = "PATH", value_parser = path_parser, value_delimiter = ':')]
    pub config_paths: Vec<PathBuf>,
    #[command(flatten)]
    pub account: AccountFlag,
    /// Force a specific backend for the cross-protocol commands.
    ///
    /// Only the shared commands (`tasklist`, `task`, `import`) read it:
    /// the protocol-specific subcommands each already name their
    /// backend.
    ///
    /// With `auto`, the first configured backend wins, in unfog's
    /// priority order (vdir, pimdir, caldav). Any other value
    /// pins that one, and the command bails when the account carries
    /// no matching block.
    #[arg(short, long, global = true, default_value_t)]
    pub backend: Backend,
    #[command(flatten)]
    pub json: JsonFlag,
    #[command(flatten)]
    pub log: LogFlags,
}

/// Every command unfog serves: the GTD family, the shared
/// cross-protocol API with the time reports, the protocol-specific
/// escape hatches, then the meta commands.
#[derive(Debug, Subcommand)]
pub enum UnfogCommand {
    #[command(subcommand)]
    Gtd(GtdCommand),
    #[command(subcommand, alias = "tasklists")]
    Tasklist(TasklistCommand),
    #[command(subcommand, alias = "tasks")]
    Task(TaskCommand),
    #[command(alias = "wtime")]
    Worktime(WorktimeCommand),
    Migrate(TimeMigrateCommand),
    Import(ImportCommand),
    #[cfg(feature = "caldav")]
    #[command(subcommand)]
    Caldav(CaldavCommand),
    #[cfg(feature = "pimdir")]
    #[command(subcommand)]
    Pimdir(PimdirCommand),
    #[cfg(feature = "vdir")]
    #[command(subcommand)]
    Vdir(VdirCommand),
    /// Configure an account interactively.
    #[cfg(all(
        feature = "wizard",
        any(feature = "caldav", feature = "vdir", feature = "pimdir")
    ))]
    #[command(visible_alias = "wizard")]
    Configure(ConfigureCommand),
    #[command(subcommand)]
    Account(AccountCommand),
    #[command(alias = "completions")]
    Completion(CompletionCommand),
    #[command(alias = "manuals")]
    Manual(ManualCommand),
    #[command(alias = "json-schemas")]
    JsonSchema(JsonSchemaCommand),
}

/// The global config and the active account's, for a shared command.
///
/// A free function rather than a closure, so the printer it needs for
/// the offer is borrowed for the length of the call rather than of the
/// whole dispatch.
pub fn resolve_account(
    printer: &mut impl Printer,
    config_paths: &[PathBuf],
    account_name: Option<&str>,
) -> Result<(Config, AccountConfig)> {
    let mut config = load_config(printer, config_paths)?;

    let Some((_, account_config)) = config.take_account(account_name)? else {
        bail!("Cannot find default account; use --account or set `default = true`")
    };

    Ok((config, account_config))
}

/// Loads the configuration from the merged `config_paths`, or explains
/// how to get one.
///
/// A missing configuration raises the offer rather than an error. The
/// wizard may print the account instead of writing it, so having run
/// it proves nothing: the lookup is repeated before failing.
pub fn load_config(printer: &mut impl Printer, config_paths: &[PathBuf]) -> Result<Config> {
    if let Some(config) = Config::from_paths_or_default(config_paths)? {
        return Ok(config);
    }

    let path = Config::target_path(config_paths)?;

    // NOTE: a cron job cannot answer a prompt and a JSON consumer wants
    // a failure it can read, so both skip the offer.
    if !printer.is_json() && stdin().is_terminal() {
        offer_configuration(printer, config_paths, &path)?;
    }

    match Config::from_paths_or_default(config_paths)? {
        Some(config) => Ok(config),
        None => bail!(
            "No configuration found at {}, {NO_CONFIG_HINT}",
            path.display()
        ),
    }
}

/// Welcomes, then offers to generate a first configuration, returning
/// whether the wizard ran.
///
/// Raised from the two places nothing can happen without a
/// configuration: a bare invocation, and a command needing an account.
/// A hook rather than a gate, so a declined offer is the caller's
/// business.
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
pub fn offer_configuration(
    printer: &mut impl Printer,
    config_paths: &[PathBuf],
    path: &Path,
) -> Result<bool> {
    wizard::configure::print_welcome(path);

    if !prompt::bool("Create a configuration with a default account?", true)? {
        return Ok(false);
    }

    ConfigureCommand.execute(printer, config_paths)?;

    Ok(true)
}

/// Reports that this build carries no wizard.
///
/// Nothing to walk the user through, so it names the missing features
/// rather than offering an empty flow.
#[cfg(not(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
)))]
pub fn offer_configuration(
    _printer: &mut impl Printer,
    _config_paths: &[PathBuf],
    _path: &Path,
) -> Result<bool> {
    eprintln!();
    eprintln!(
        "This build carries no wizard, which needs the `wizard` feature and one of \
         caldav, vdir or pimdir; write a configuration by hand, starting from \
         config.sample.toml"
    );
    eprintln!();

    Ok(false)
}

impl UnfogCommand {
    /// Runs the subcommand against the account `-a` names, or the
    /// default one.
    pub fn execute(
        self,
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
        backend: Backend,
    ) -> Result<()> {
        let scope = |printer: &mut _| Scope::resolve(printer, config_paths, account_name, backend);

        match self {
            Self::Gtd(cmd) => cmd.execute(printer, config_paths, account_name, backend),
            Self::Tasklist(cmd) => {
                let client = TaskClient::resolve(printer, config_paths, account_name, backend)?;
                cmd.execute(printer, client)
            }
            Self::Task(cmd) => cmd.execute(printer, config_paths, account_name, backend),
            Self::Worktime(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Migrate(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Import(cmd) => {
                let client = TaskClient::resolve(printer, config_paths, account_name, backend)?;
                cmd.execute(printer, client)
            }

            #[cfg(feature = "caldav")]
            Self::Caldav(cmd) => {
                let client = build_caldav_client(printer, config_paths, account_name)?;
                cmd.execute(printer, client)
            }
            #[cfg(feature = "pimdir")]
            Self::Pimdir(cmd) => {
                let backend = PimdirBackend::build(printer, config_paths, account_name)?;
                cmd.execute(printer, backend)
            }
            #[cfg(feature = "vdir")]
            Self::Vdir(cmd) => {
                let client = build_vdir_client(printer, config_paths, account_name)?;
                cmd.execute(printer, client)
            }

            #[cfg(all(
                feature = "wizard",
                any(feature = "caldav", feature = "vdir", feature = "pimdir")
            ))]
            Self::Configure(cmd) => cmd.execute(printer, config_paths),
            Self::Account(cmd) => cmd.execute(printer, config_paths, account_name, backend),
            Self::Completion(cmd) => cmd.execute(printer, UnfogCli::command()),
            Self::Manual(cmd) => cmd.execute(printer, UnfogCli::command()),
            Self::JsonSchema(cmd) => cmd.execute(printer, json_schema::registry()),
        }
    }
}
