//! # Vdir
//!
//! The vdir backend: the shared-API adapter over io-vdir, the client
//! rooted at a home directory, and the `vdir` commands.

pub mod backend;
pub mod cli;
pub mod client;
pub mod create;
pub mod delete;
pub mod list;
pub mod rename;
