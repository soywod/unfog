//! # Tasklist
//!
//! The collection shared by every backend, and the `tasklist` command
//! family built on it.
//!
//! [`Tasklist`] is unfog's own least-common-denominator shape, no library
//! sitting between the CLI and the io-* crates. Each backend adapter projects
//! its native collection onto it: a calendar taking VTODOs, a vdir
//! collection, a pimdir collection.

pub mod cli;
pub mod create;
pub mod delete;
pub mod list;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// A collection holding tasks.
///
/// Partial-coverage fields stay optional, populated only by the backends that
/// know them.
#[derive(Clone, Debug, Default, Eq, PartialEq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Tasklist {
    /// Backend-specific identifier the other commands address it by.
    pub id: String,
    /// Human-readable display name, falling back to the id.
    pub name: String,
    /// Free-form description, when the backend exposes one.
    #[serde(default)]
    pub description: Option<String>,
    /// ASCII `#RRGGBB` color marker, when the backend exposes one.
    #[serde(default)]
    pub color: Option<String>,
    /// Whether new items go here when no collection is named, as the
    /// backend states it; `false` where the backend does not say.
    #[serde(default)]
    pub default: bool,
}
