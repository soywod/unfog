//! # Shared arguments
//!
//! Clap arguments reused across the shared API commands.

use clap::Parser;

/// The tasklist a shared-API command operates on.
///
/// The id resolves through the account: the flag wins, then
/// `tasklist.default`, then the command bails.
#[derive(Debug, Parser)]
pub struct TasklistIdArg {
    /// Tasklist the command operates on.
    ///
    /// Falls back to the `tasklist.default` config when omitted,
    /// otherwise the command bails.
    #[arg(short = 'l', long = "tasklist", value_name = "TASKLIST-ID")]
    pub id: Option<String>,
}

/// The composer a `create` or an `update` refines a task in.
#[derive(Debug, Parser)]
pub struct IcalComposerArgs {
    /// Edit the task in the composer before writing it.
    ///
    /// Bails when neither `task.composer` nor `--composer` names one.
    #[arg(short, long)]
    pub interactive: bool,
    /// Command the task is edited in, overriding `task.composer`.
    ///
    /// A shell line, spawned on the path of a temporary iCalendar file it
    /// edits in place.
    #[arg(long, value_name = "COMMAND", requires = "interactive")]
    pub composer: Option<String>,
}
