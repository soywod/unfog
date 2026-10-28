//! # Pimdir
//!
//! unfog over a local [pimdir] store: an offline cache, a SQLite
//! index plus content-addressed blobs, that a sync engine fills rather
//! than a live server.
//!
//! Reads project the store's items through [`PimdirReader`], which
//! takes no lock: an item whose body is not local lists from its
//! summary and reads as "body not fetched", the cue to sync. It also
//! overlays the queue (pimdir SPEC 15.4), so a staged change shows.
//!
//! Writes are staged as queue actions for the store's owner to apply
//! and push. A staged creation shows in no listing, having no public id
//! until the owner applies it, which is what `pimdir status` counts.
//!
//! Collections come from the sync, so the collection verbs (create,
//! update, delete) are not served here: a cache does not invent
//! collections its source does not have.
//!
//! [pimdir]: https://github.com/pimalaya/pimdir
//! [`PimdirReader`]: io_pimdir::client::reader::PimdirReader

pub mod backend;
pub mod cli;
pub mod client;
pub mod status;
