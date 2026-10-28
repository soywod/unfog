//! # Local backend wizard
//!
//! A typed path pointing at an existing folder configures a local backend,
//! its kind auto-detected from the directory's own markers: a pimdir store
//! carries its SQLite index and blob tree, a vdir home one subdirectory per
//! calendar.
//!
//! When detection is inconclusive and both backends are compiled in, the user
//! picks; otherwise the sole compiled backend is used.

use std::path::{Path, PathBuf};

use anyhow::Result;

#[cfg(feature = "pimdir")]
use crate::config::PimdirConfig;
#[cfg(feature = "vdir")]
use crate::config::VdirConfig;

/// The file a pimdir store keeps its index in.
#[cfg(feature = "pimdir")]
const PIMDIR_INDEX: &str = "pimdir.db";

/// The directory a pimdir store keeps its content-addressed bodies in.
#[cfg(feature = "pimdir")]
const PIMDIR_OBJECTS: &str = "objects";

/// A configured local backend.
pub enum Local {
    /// A vdir home, one directory per calendar.
    #[cfg(feature = "vdir")]
    Vdir(VdirConfig),
    /// A pimdir store, the offline store a sync fills.
    #[cfg(feature = "pimdir")]
    Pimdir(PimdirConfig),
}

/// Configures a local backend rooted at `root`, auto-detecting its kind from
/// the on-disk markers and prompting only when that is inconclusive.
pub fn configure(root: PathBuf) -> Result<Local> {
    match detect(&root) {
        Some(local) => Ok(local),
        None => pick(root),
    }
}

/// Detects the backend kind from `root`'s markers.
///
/// pimdir is tested first, on its index file: a vdir home is just
/// directories, so anything holding a `pimdir.db` is a store rather than a
/// home that happens to contain one.
#[cfg_attr(
    not(all(feature = "vdir", feature = "pimdir")),
    allow(unused_variables)
)]
fn detect(root: &Path) -> Option<Local> {
    #[cfg(feature = "pimdir")]
    if root.join(PIMDIR_INDEX).is_file() || root.join(PIMDIR_OBJECTS).is_dir() {
        return Some(Local::Pimdir(PimdirConfig {
            root: root.to_path_buf(),
            account: None,
        }));
    }

    #[cfg(feature = "vdir")]
    if has_collection(root) {
        return Some(Local::Vdir(VdirConfig {
            home_dir: root.to_path_buf(),
        }));
    }

    None
}

/// Whether `root` holds at least one vdir collection: a subdirectory carrying
/// an item or a metadata marker.
///
/// An empty directory is deliberately not a match, so the ambiguous case
/// reaches the prompt.
#[cfg(feature = "vdir")]
fn has_collection(root: &Path) -> bool {
    let Ok(entries) = root.read_dir() else {
        return false;
    };

    entries.flatten().any(|entry| {
        entry.path().is_dir()
            && entry
                .path()
                .read_dir()
                .map(|inner| {
                    inner.flatten().any(|item| {
                        let name = item.file_name();
                        let name = name.to_string_lossy();
                        name.ends_with(".ics")
                            || matches!(name.as_ref(), "displayname" | "description" | "color")
                    })
                })
                .unwrap_or(false)
    })
}

#[cfg(all(feature = "vdir", feature = "pimdir"))]
fn pick(root: PathBuf) -> Result<Local> {
    use pimalaya_cli::prompt;

    const VDIR: &str = "vdir (one directory per tasklist)";
    const PIMDIR: &str = "pimdir (an offline store a sync fills)";

    Ok(
        match prompt::item("Local backend:", [VDIR, PIMDIR], None)? {
            VDIR => Local::Vdir(VdirConfig { home_dir: root }),
            _ => Local::Pimdir(PimdirConfig {
                root,
                account: None,
            }),
        },
    )
}

#[cfg(all(feature = "vdir", not(feature = "pimdir")))]
fn pick(root: PathBuf) -> Result<Local> {
    Ok(Local::Vdir(VdirConfig { home_dir: root }))
}

#[cfg(all(feature = "pimdir", not(feature = "vdir")))]
fn pick(root: PathBuf) -> Result<Local> {
    Ok(Local::Pimdir(PimdirConfig {
        root,
        account: None,
    }))
}

#[cfg(test)]
mod tests {
    use std::{env::temp_dir, fs};

    use super::*;

    fn scratch(name: &str) -> PathBuf {
        let dir = temp_dir().join(format!("unfog-wizard-local-{name}"));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[cfg(feature = "pimdir")]
    #[test]
    fn an_index_file_marks_a_pimdir_store() {
        let root = scratch("store");
        fs::write(root.join(PIMDIR_INDEX), b"").unwrap();

        assert!(matches!(detect(&root), Some(Local::Pimdir(_))));
    }

    #[cfg(feature = "vdir")]
    #[test]
    fn a_directory_of_collections_marks_a_vdir_home() {
        let root = scratch("home");
        let collection = root.join("personal");
        fs::create_dir_all(&collection).unwrap();
        fs::write(collection.join("event.ics"), b"BEGIN:VCALENDAR\r\n").unwrap();

        assert!(matches!(detect(&root), Some(Local::Vdir(_))));
    }

    #[test]
    fn an_empty_directory_stays_ambiguous_and_reaches_the_prompt() {
        let root = scratch("empty");
        assert!(detect(&root).is_none());
    }

    #[cfg(all(feature = "vdir", feature = "pimdir"))]
    #[test]
    fn a_store_inside_a_home_still_reads_as_a_store() {
        // NOTE: a pimdir root also holds subdirectories, so testing vdir
        // first would misread every store as a home.
        let root = scratch("both");
        fs::write(root.join(PIMDIR_INDEX), b"").unwrap();
        let collection = root.join("personal");
        fs::create_dir_all(&collection).unwrap();
        fs::write(collection.join("event.ics"), b"BEGIN:VCALENDAR\r\n").unwrap();

        assert!(matches!(detect(&root), Some(Local::Pimdir(_))));
    }
}
