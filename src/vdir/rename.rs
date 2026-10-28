//! # Vdir collection rename
//!
//! The `unfog vdir rename` command, renaming one collection
//! directory in place.

use anyhow::Result;
use clap::Parser;
use io_vdir::path::VdirPath;
use pimalaya_cli::printer::{Message, Printer};

use crate::vdir::client::VdirClient;

/// Rename a vdir collection directory.
///
/// The id is the directory name, so this renames the directory itself
/// and leaves the item files inside it untouched.
///
/// JSON output: `{"message": "..."}`.
#[derive(Debug, Parser)]
pub struct VdirCollectionRenameCommand {
    /// Current collection identifier (directory name).
    #[arg(value_name = "OLD-ID")]
    pub id: String,

    /// New collection identifier (directory name).
    #[arg(value_name = "NEW-ID")]
    pub new_id: String,
}

impl VdirCollectionRenameCommand {
    pub fn execute(self, printer: &mut impl Printer, client: VdirClient) -> Result<()> {
        let path = VdirPath::new(client.root().as_str()).join(&self.id);
        let new_id = self.new_id.clone();
        client.rename_collection(path, self.new_id)?;

        let msg = format!(
            "Collection `{}` successfully renamed to `{new_id}`",
            self.id
        );
        printer.out(Message::new(msg))
    }
}
