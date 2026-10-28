//! # GTD command
//!
//! The `gtd` command family: capture, the four views, clarify and the
//! review.

use std::path::PathBuf;

use anyhow::Result;
use clap::Subcommand;
use pimalaya_cli::printer::Printer;

use crate::{
    backend::Backend,
    gtd::{
        capture::CaptureCommand,
        clarify::ClarifyCommand,
        review::ReviewCommand,
        view::{InboxCommand, NextCommand, SomedayCommand, WaitingCommand},
    },
    scope::Scope,
    shared::client::TaskClient,
};

/// Get things done: capture, clarify, the views and the review.
///
/// Every command but `capture` spans every configured account; `-a`
/// narrows it to one.
#[derive(Debug, Subcommand)]
pub enum GtdCommand {
    Capture(CaptureCommand),
    Inbox(InboxCommand),
    Next(NextCommand),
    Waiting(WaitingCommand),
    Someday(SomedayCommand),
    Clarify(ClarifyCommand),
    Review(ReviewCommand),
}

impl GtdCommand {
    pub fn execute(
        self,
        printer: &mut impl Printer,
        config_paths: &[PathBuf],
        account_name: Option<&str>,
        backend: Backend,
    ) -> Result<()> {
        let scope = |printer: &mut _| Scope::resolve(printer, config_paths, account_name, backend);

        match self {
            Self::Capture(cmd) => {
                let client = TaskClient::resolve(printer, config_paths, account_name, backend)?;
                cmd.execute(printer, client)
            }
            Self::Inbox(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Next(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Waiting(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Someday(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Clarify(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
            Self::Review(cmd) => {
                let scope = scope(printer)?;
                cmd.execute(printer, scope)
            }
        }
    }
}
