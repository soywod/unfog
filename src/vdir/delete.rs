//! # Vdir collection delete
//!
//! The `unfog vdir delete` command, removing one collection
//! directory and everything inside it.

use anyhow::Result;
use clap::Parser;
use io_vdir::path::VdirPath;
use pimalaya_cli::printer::{Message, Printer};

use crate::vdir::client::VdirClient;

/// Delete a vdir collection.
///
/// Removes the collection directory and every item file in it, without
/// asking and without a trash to recover them from.
///
/// JSON output: `{"message": "..."}`.
#[derive(Debug, Parser)]
pub struct VdirCollectionDeleteCommand {
    /// Collection identifier (directory name).
    #[arg(value_name = "ID")]
    pub id: String,
}

impl VdirCollectionDeleteCommand {
    pub fn execute(self, printer: &mut impl Printer, client: VdirClient) -> Result<()> {
        let path = VdirPath::new(client.root().as_str()).join(&self.id);
        client.delete_collection(path)?;

        let msg = format!("Collection `{}` successfully deleted", self.id);
        printer.out(Message::new(msg))
    }
}
