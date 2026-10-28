//! # Pimdir client
//!
//! unfog's wrapper around [`io_pimdir`]'s reader and producer
//! roles. The store belongs to the sync engine, not to unfog.
//!
//! Reads go through [`PimdirReader`], which takes no lock (pimdir SPEC
//! 8) and carries no write, so a sync in flight neither blocks unfog
//! nor is blocked by it. Writes go through [`PimdirProducer`], which
//! takes the shared lock for one enqueue and never mutates the index.
//!
//! The reader folds the queue's pending actions over the committed rows
//! (pimdir SPEC 15.4), so an action this process staged reads back
//! before the store's owner applies it.

use std::path::PathBuf;

use anyhow::{Result, anyhow};
use io_pimdir::client::{blobs::PimdirBlobs, producer::PimdirProducer, reader::PimdirReader};

use crate::config::PimdirConfig;

/// The process name each staged action records (pimdir SPEC 15.1).
///
/// Diagnostic only: it says who asked, never who applies.
const PRODUCER: &str = "unfog";

/// A live pimdir client: a lock-free reader over the store and blobs.
///
/// A write opens a producer of its own and drops it, so this handle
/// never holds anything a sync has to wait on.
pub struct PimdirClient {
    /// The lock-free reader every read goes through.
    pub(crate) reader: PimdirReader,
    /// The blob store item bodies are read from and staged into.
    pub(crate) blobs: PimdirBlobs,
    /// The expanded store root, which a producer is opened against.
    root: PathBuf,
    /// The account collections are grouped under, `None` when the store
    /// groups none.
    pub(crate) account: Option<String>,
}

impl PimdirClient {
    /// Opens the pimdir store at the configured root to read.
    ///
    /// The store must exist: a reader creates nothing, the schema being
    /// the owner's to write, so a root holding no store fails here
    /// rather than listing an empty calendar set.
    ///
    /// The root arrives shell-expanded, [`PimdirConfig::root`] doing it
    /// at deserialize.
    pub fn new(config: PimdirConfig) -> Result<Self> {
        let root = config.root.clone();

        let reader = PimdirReader::open(&root)
            .map(PimdirReader::with_pending)
            .map_err(|err| anyhow!("Open pimdir store `{}`: {err}", root.display()))?;
        let blobs = reader.blobs();

        Ok(Self {
            reader,
            blobs,
            root,
            account: config.account.clone(),
        })
    }

    /// Opens a producer for one staging window.
    ///
    /// The enqueue-only role takes the shared lock, not the owner's
    /// exclusive one, so it keeps no sync out. Opened per write: the
    /// lock only buys the window between a body landing and its row.
    pub(crate) fn producer(&self) -> Result<PimdirProducer> {
        let producer = PimdirProducer::open(&self.root, PRODUCER)
            .map_err(|err| anyhow!("Stage into pimdir store `{}`: {err}", self.root.display()))?;

        Ok(match self.account.clone() {
            Some(account) => producer.for_account(account),
            None => producer,
        })
    }
}
