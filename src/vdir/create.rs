//! # Vdir collection create
//!
//! The `unfog vdir create` command, making one collection
//! directory under the configured home directory.

use anyhow::Result;
use clap::Parser;
use io_vdir::{collection::VdirCollection, path::VdirPath};
use pimalaya_cli::printer::{Message, Printer};

use crate::vdir::client::VdirClient;

/// Create a vdir collection directory.
///
/// The directory is made under the configured home, and each metadata
/// flag below writes the marker file vdir reads that value from.
///
/// JSON output: `{"message": "..."}`.
#[derive(Debug, Parser)]
pub struct VdirCollectionCreateCommand {
    /// Collection identifier (directory name).
    #[arg(value_name = "ID")]
    pub id: String,

    /// Display name, written to the `displayname` marker file.
    #[arg(short, long, value_name = "NAME")]
    pub display_name: Option<String>,

    /// Description, written to the `description` marker file.
    #[arg(short = 'D', long, value_name = "TEXT")]
    pub description: Option<String>,

    /// Hex color (`#RRGGBB`), written to the `color` marker file.
    #[arg(long, value_name = "HEX")]
    pub color: Option<String>,
}

impl VdirCollectionCreateCommand {
    pub fn execute(self, printer: &mut impl Printer, client: VdirClient) -> Result<()> {
        let path = VdirPath::new(client.root().as_str()).join(&self.id);
        let collection = VdirCollection {
            path,
            display_name: self.display_name,
            description: self.description,
            color: self.color,
        };

        client.create_collection(collection)?;

        let msg = format!("Collection `{}` successfully created", self.id);
        printer.out(Message::new(msg))
    }
}
