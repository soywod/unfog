//! # Unfog
//!
//! A CLI to manage tasks, one VTODO (RFC 5545 3.6.2) each, wherever a
//! backend keeps them: a CalDAV server, a vdir folder, a pimdir store. No
//! library target, no protocol or storage logic of its own: a thin shell
//! driving the sans-I/O libraries below and owning the cross-protocol
//! abstraction over them.
//!
//! This header is the architecture document of the crate. The living
//! specification, the in-flight proposals and the landed history are in
//! the cairn folder, one file per capability under cairn/spec.
//!
//! ## The stack below
//!
//! [`io_webdav`] carries the CalDAV coroutines (RFC 4791 over RFC 4918),
//! [`io_vdir`] the local vdir filesystem, [`io_pimdir`] the offline store
//! and the sync engine that fills it.
//!
//! [`io_http`] is the request and response state machines WebDAV runs on,
//! `io_pim_discovery` the RFC 6764 CalDAV discovery, and `ical` the
//! iCalendar parser and editor.
//!
//! The `pimalaya_cli`, `pimalaya_config` and `pimalaya_stream` toolkits
//! supply the clap scaffolding, the TOML loading and secrets, and the
//! blocking transport. Every coroutine lives in those libraries and unfog
//! implements none.
//!
//! ## The cross-protocol layer unfog owns
//!
//! There is no aggregator crate between the CLI and the io-* libraries.
//! unfog owns its own least-common-denominator types
//! ([`shared::tasklist::Tasklist`], [`shared::item::TaskItem`]) and its own
//! dispatcher ([`shared::client::TaskClient`]), holding exactly one
//! backend. The shape is calendula's, copied rather than shared.
//!
//! Each protocol module carries a backend submodule adapting that library
//! to the shared surface, so an operation only reaches the shared API when
//! every backend can serve it. The three backends speak iCalendar natively
//! and store a task's bytes verbatim.
//!
//! ## Tasks, GTD, and time entries
//!
//! unfog reads and writes VTODO components, and drops every other kind a
//! task listing meets. A field flag rewrites the property it names and
//! leaves every other line of a task byte for byte, so whatever another
//! client keeps on a task survives unfog.
//!
//! GTD is the convention pimdir's guide states (GUIDE.md §17): three
//! reserved `CATEGORIES` values, `inbox`, `waiting` and `someday`, every
//! other category the person's own ([`shared::task::gtd`]). The views
//! ([`gtd`]) span every account of the [`scope`], and a writer edits only
//! the three values.
//!
//! Time tracking ([`time`]) is the one place unfog writes another kind: a
//! time entry is a VEVENT, or a pair of VJOURNALs, tied to its task by
//! `RELATED-TO`, kept where the account's `time.strategy` says.
//!
//! A task is addressed by its item's handle or by a prefix of its `UID`,
//! dashes ignored, as unfog 1.x did ([`shared::task::find`]).
//!
//! ## Four command families
//!
//! The command tree ([`cli`]) is split into four groups, in this order.
//! The `gtd` family (`capture`, `inbox`, `next`, `waiting`, `someday`,
//! `clarify`, `review`) is the method unfog serves, across every
//! account. The shared API (`tasklist`, `task`, `worktime`, `migrate`,
//! `import`) is the portable surface, `task start`, `task stop` and
//! `task status` tracking time across every account.
//! The protocol-specific APIs (`caldav`, `pimdir`, `vdir`) each expose what
//! only one backend has, gated behind its own cargo feature. The meta
//! commands (`configure`, `account`, `completion`, `manual`, `json-schema`)
//! inspect the configuration and generate the shell integration.
//!
//! ## Backend selection
//!
//! The shared commands target the backend the global `--backend` flag
//! picks ([`backend::Backend`]). Its default, `auto`, takes the first
//! configured-and-compiled backend in the order vdir, pimdir, CalDAV: a
//! local read before a network round-trip.
//!
//! ## Import of the 1.x store
//!
//! unfog 1.x kept its tasks in an append-only log of Haskell `Show` values.
//! [`import`] folds it into one VTODO per task, its project a plain
//! category, and pairs its starts and stops into time entries of the
//! account's strategy.
//!
//! ## Configuration and the wizard
//!
//! Configuration is loaded by pimalaya-config from the first existing
//! canonical path, or from `-c` / `UNFOG_CONFIG`, with later paths
//! deep-merged on top of the first. The schema is multi-account: a
//! top-level block plus named account blocks, each carrying an optional
//! sub-block per backend, and the global block is folded under the
//! selected account.
//!
//! `unfog configure` runs the wizard, which is also offered where nothing
//! can happen without a configuration. A bare `unfog` finding a
//! configuration prints the help instead.
//!
//! ## Output
//!
//! All data and errors go to stdout through the printer, with `--json`
//! switching every command to JSON; stderr carries logs and prompts only.
//! A command returns a `Serialize + Display` value rather than printing
//! inline, and each command's doc comment is its help text.

mod account;
mod backend;
#[cfg(feature = "caldav")]
mod caldav;
mod cli;
mod config;
mod error;
mod gtd;
mod import;
mod json_schema;
#[cfg(feature = "pimdir")]
mod pimdir;
mod scope;
mod shared;
#[cfg(test)]
mod testing;
mod time;
#[cfg(feature = "vdir")]
mod vdir;
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
mod wizard;

use std::{
    io::{IsTerminal, stdin},
    path::PathBuf,
};

use anyhow::Result;
use clap::{CommandFactory, Parser};
use pimalaya_cli::{
    error::ErrorReport,
    log::Logger,
    printer::{Printer, StdoutPrinter},
};
use pimalaya_config::toml::TomlConfig;

use crate::{cli::UnfogCli, config::Config, error::ErrorCode};

fn main() {
    let cli = UnfogCli::parse();
    let mut printer = StdoutPrinter::new(&cli.json);
    let result = run(cli, &mut printer);
    ErrorReport::<ErrorCode>::eval_coded(&mut printer, result);
}

fn run(cli: UnfogCli, printer: &mut StdoutPrinter) -> Result<()> {
    Logger::try_init(&cli.log)?;

    let config_paths = cli.config_paths.as_ref();
    let account = cli.account.name.as_deref();
    let backend = cli.backend;

    let Some(command) = cli.command else {
        return meet_bare_invocation(printer, config_paths, account.is_some());
    };

    command.execute(printer, config_paths, account, backend)
}

/// Meets a bare `unfog`, which is where a newcomer lands.
///
/// A missing configuration raises the offer and everything else gets the
/// help: an existing configuration, a script, a JSON caller, and `--account`,
/// which reads as a half-typed command. A file that exists but fails to parse
/// counts as a configuration, so the offer never writes over a broken one.
fn meet_bare_invocation(
    printer: &mut StdoutPrinter,
    config_paths: &[PathBuf],
    named_account: bool,
) -> Result<()> {
    let configured = Config::from_paths_or_default(config_paths)
        .ok()
        .flatten()
        .is_some();

    if !configured && !named_account && !printer.is_json() && stdin().is_terminal() {
        let path = Config::target_path(config_paths)?;

        // NOTE: nothing runs after the offer, so a declined one falls back
        // to the help; the wizard says what to run next when it ran.
        if cli::offer_configuration(printer, config_paths, &path)? {
            return Ok(());
        }
    }

    UnfogCli::command().print_help()?;

    Ok(())
}
