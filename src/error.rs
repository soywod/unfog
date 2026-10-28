//! # Error codes
//!
//! A stable, machine-readable name for the failures a caller is expected
//! to act on, printed as `code` beside the message under `--json`.
//!
//! The message stays for humans and may be reworded at any time; the
//! code is the contract. A failure with no code prints as before, with
//! no `code` field. The mechanism (`CodedError`, `code_of`, the report)
//! lives in `pimalaya_cli::error`; only the vocabulary is unfog's.

use schemars::JsonSchema;
use serde::Serialize;

/// The stable codes, each serialised in kebab-case.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum ErrorCode {
    /// The item is listed but its body is not local yet: a sync will
    /// bring it, so the caller waits rather than reports a loss.
    BodyPending,
    /// A write named, by `--if-match`, a version the item no longer has:
    /// the caller reads it again rather than overwrites a change.
    PreconditionFailed,
}
