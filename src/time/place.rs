//! # Time places
//!
//! Where an account's time entries live: the tasklist `time.collection`
//! names on its backend (the `event` and `journal` strategies), and the
//! place the `local` strategy keeps on the device, never synced: the
//! local time collection of a pimdir store (`U+0001time`, pimdir STORAGE
//! §14.3), else a vdir directory.

use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, anyhow, bail};
use log::debug;

use crate::{
    config::TimeStrategy,
    shared::client::TaskClient,
    time::entry::{RawItem, Shape, uid_of},
};

/// A place time entries live in.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TimePlace {
    /// A tasklist of the account's backend, synced with it.
    Collection(String),
    /// The local time collection of the pimdir store at this root.
    #[cfg_attr(not(feature = "pimdir"), allow(dead_code))]
    Pimdir(PathBuf),
    /// A vdir directory of `.ics` files.
    Directory(PathBuf),
}

impl TimePlace {
    /// The place the `local` strategy of `client`'s account keeps its
    /// entries in.
    pub fn local(client: &TaskClient) -> Result<Self> {
        if let Some(root) = client.pimdir_root() {
            return Ok(Self::Pimdir(root));
        }

        Ok(Self::Directory(client.account.time_directory()?))
    }

    /// Every place `client`'s account keeps entries in: the local one,
    /// then `time.collection` when set, so a history survives a switch of
    /// strategy.
    pub fn every(client: &TaskClient) -> Result<Vec<Self>> {
        let mut places = vec![Self::local(client)?];

        if let Some(collection) = &client.account.time_collection {
            places.push(Self::Collection(collection.clone()));
        }

        Ok(places)
    }

    /// Where `strategy` writes the entries of `client`'s account, and in
    /// which shape.
    pub fn of(client: &TaskClient, strategy: TimeStrategy) -> Result<(Self, Shape)> {
        let collection = || match &client.account.time_collection {
            Some(collection) => Ok(Self::Collection(collection.clone())),
            None => Err(anyhow!(
                "The `{strategy}` time strategy writes to a calendar; set time.collection to \
                 one of the account's tasklists"
            )),
        };

        match strategy {
            TimeStrategy::Event => Ok((collection()?, Shape::Event)),
            TimeStrategy::Journal => Ok((collection()?, Shape::Journal)),
            TimeStrategy::Local => Ok((Self::local(client)?, Shape::Event)),
        }
    }

    /// How a report names the place.
    pub fn describe(&self) -> String {
        match self {
            Self::Collection(id) => format!("tasklist `{id}`"),
            Self::Pimdir(root) => format!("the local time collection of {}", root.display()),
            Self::Directory(dir) => format!("{}", dir.display()),
        }
    }

    /// Lists the items of the place.
    ///
    /// A place nothing was written to yet holds nothing.
    pub fn list(&self, client: &mut TaskClient) -> Result<Vec<RawItem>> {
        match self {
            Self::Collection(id) => Ok(client
                .list_items(id, Default::default())?
                .into_iter()
                .map(|item| RawItem {
                    id: item.id,
                    etag: item.etag,
                    contents: item.contents,
                })
                .collect()),
            Self::Pimdir(root) => pimdir::list(root),
            Self::Directory(dir) => directory::list(dir),
        }
    }

    /// Writes a new entry item, returning its handle.
    pub fn create(&self, client: &mut TaskClient, contents: Vec<u8>) -> Result<String> {
        match self {
            Self::Collection(id) => client.create_item(id, contents),
            Self::Pimdir(root) => pimdir::put(root, &contents),
            Self::Directory(dir) => directory::create(dir, &contents),
        }
    }

    /// Replaces an entry item, gated on `etag` where the place keeps one.
    pub fn update(&self, client: &mut TaskClient, item: &RawItem, contents: Vec<u8>) -> Result<()> {
        match self {
            Self::Collection(id) => {
                client.update_item(id, &item.id, contents, item.etag.as_deref())
            }
            Self::Pimdir(root) => {
                // NOTE: a body under the key the collection holds
                // replaces it (io-pimdir `put_item`).
                pimdir::put(root, &contents).map(|_| ())
            }
            Self::Directory(dir) => directory::write(&dir.join(&item.id), &contents),
        }
    }

    /// Removes an entry item.
    pub fn delete(&self, client: &mut TaskClient, item: &RawItem) -> Result<()> {
        match self {
            Self::Collection(id) => client.delete_item(id, &item.id, item.etag.as_deref()),
            Self::Pimdir(root) => pimdir::delete(root, &item.id),
            Self::Directory(dir) => fs::remove_file(dir.join(&item.id))
                .with_context(|| format!("Remove time entry {}", dir.join(&item.id).display())),
        }
    }
}

/// The local time collection of a pimdir store.
#[cfg(feature = "pimdir")]
mod pimdir {
    use std::path::Path;

    use anyhow::{Result, anyhow};
    use io_pimdir::{
        client::{PimdirStore, TIME_COLLECTION, reader::PimdirReader},
        placement::PimdirLinkId,
    };

    use crate::time::entry::RawItem;

    /// How many rows one page of the listing reads.
    const BATCH: usize = 500;

    /// Lists the entries, read without a lock as any reader does.
    pub fn list(root: &Path) -> Result<Vec<RawItem>> {
        let reader = PimdirReader::open(root)
            .map_err(|err| anyhow!("Open pimdir store `{}`: {err}", root.display()))?;
        let blobs = reader.blobs();
        let mut items = Vec::new();
        let mut cursor: Option<(String, i64)> = None;

        loop {
            let batch = reader.list_summaries(
                TIME_COLLECTION,
                cursor.as_ref().map(|(key, seq)| (key.as_str(), *seq)),
                BATCH,
            )?;
            let count = batch.len();

            if let Some(last) = batch.last() {
                cursor = Some((last.sort_key.clone(), last.seq));
            }

            for stored in batch {
                let Some(hash) = stored.object else {
                    continue;
                };
                let Some(contents) = blobs.get(&hash)? else {
                    continue;
                };

                items.push(RawItem {
                    id: stored.link_id.0,
                    etag: Some(hash.0),
                    contents,
                });
            }

            if count < BATCH {
                return Ok(items);
            }
        }
    }

    /// Writes an entry as the store's owner, its key derived from its
    /// body, declaring the collection first.
    ///
    /// The owner's lock is exclusive: a sync running on the store makes
    /// this fail at once rather than wait.
    pub fn put(root: &Path, contents: &[u8]) -> Result<String> {
        let mut store = owner(root)?;
        store.ensure_time_collection()?;
        let (link, _) = store.put_item(TIME_COLLECTION, contents)?;
        Ok(link.0)
    }

    /// Deletes an entry as the store's owner.
    pub fn delete(root: &Path, id: &str) -> Result<()> {
        let mut store = owner(root)?;
        store.delete_unbound(TIME_COLLECTION, &PimdirLinkId(id.to_owned()))?;
        Ok(())
    }

    fn owner(root: &Path) -> Result<PimdirStore> {
        PimdirStore::open(root).map_err(|err| {
            anyhow!(
                "Write the local time collection of pimdir store `{}`: {err}",
                root.display()
            )
        })
    }
}

/// Stands in for the pimdir place in a build without pimdir: never
/// reached, [`TimePlace::local`] only naming it on that backend.
#[cfg(not(feature = "pimdir"))]
mod pimdir {
    use std::path::Path;

    use anyhow::{Result, bail};

    use crate::time::entry::RawItem;

    pub fn list(_root: &Path) -> Result<Vec<RawItem>> {
        bail!("This build carries no pimdir backend")
    }

    pub fn put(_root: &Path, _contents: &[u8]) -> Result<String> {
        bail!("This build carries no pimdir backend")
    }

    pub fn delete(_root: &Path, _id: &str) -> Result<()> {
        bail!("This build carries no pimdir backend")
    }
}

/// A vdir directory of `.ics` files, one entry item each, named after its
/// `UID`.
mod directory {
    use super::*;

    /// Lists the `.ics` files of `dir`, none when it does not exist yet.
    pub fn list(dir: &Path) -> Result<Vec<RawItem>> {
        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
                debug!("no time directory at {} yet", dir.display());
                return Ok(Vec::new());
            }
            Err(err) => {
                return Err(err).with_context(|| format!("List time entries of {}", dir.display()));
            }
        };

        let mut items = Vec::new();

        for entry in entries {
            let path = entry?.path();

            if path.extension().is_none_or(|ext| ext != "ics") || !path.is_file() {
                continue;
            }

            let Some(id) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };

            items.push(RawItem {
                id: id.to_owned(),
                etag: None,
                contents: fs::read(&path)
                    .with_context(|| format!("Read time entry {}", path.display()))?,
            });
        }

        items.sort_by(|a, b| a.id.cmp(&b.id));
        Ok(items)
    }

    /// Writes a new item under a name derived from its `UID`, refusing
    /// one that exists.
    pub fn create(dir: &Path, contents: &[u8]) -> Result<String> {
        let Some(uid) = uid_of(contents) else {
            bail!("A time entry carries no UID");
        };

        let name: String = uid
            .chars()
            .map(|c| match c.is_ascii_alphanumeric() || "-_.@".contains(c) {
                true => c,
                false => '_',
            })
            .collect();
        let name = format!("{name}.ics");
        let path = dir.join(&name);

        fs::create_dir_all(dir)
            .with_context(|| format!("Create time directory {}", dir.display()))?;

        if path.exists() {
            bail!("Time entry {} exists already", path.display());
        }

        write(&path, contents)?;
        Ok(name)
    }

    /// Writes `contents` at `path` through a sibling file renamed over
    /// it, so a reader never sees half an entry.
    pub fn write(path: &Path, contents: &[u8]) -> Result<()> {
        let staged = path.with_extension("ics.tmp");

        fs::write(&staged, contents)
            .with_context(|| format!("Write time entry {}", staged.display()))?;
        fs::rename(&staged, path).with_context(|| format!("Write time entry {}", path.display()))
    }
}
