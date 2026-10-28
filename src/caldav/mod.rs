//! # CalDAV
//!
//! The CalDAV backend: the shared-API adapter over io-webdav, the
//! connected client, and the protocol-specific `caldav` commands.

pub mod backend;
pub mod cli;
pub mod client;
pub mod create;
pub mod delete;
pub mod discover;
pub mod list;
