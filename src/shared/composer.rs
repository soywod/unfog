//! # Item composer
//!
//! The command an item is edited through, and the temporary file it is
//! handed.
//!
//! unfog writes the iCalendar to a file, spawns the configured
//! command on its path with every stream inherited, and reads the file
//! back once the command exits. Nothing is captured, which is the whole
//! point: a composer spawning an editor would otherwise hand it a pipe
//! instead of the terminal, and the editor hangs or writes where nothing
//! reads.

use std::{
    env::temp_dir,
    fs,
    path::{Path, PathBuf},
    process::{ExitStatus, Stdio},
};

use anyhow::{Context, Error, Result, anyhow, bail};
use log::{debug, info};
use pimalaya_cli::{printer::Printer, prompt};
use pimalaya_config::command::{self, CommandConfig};

use crate::shared::{ical::check, uuid::uuid_v4};

/// The command an item is edited through.
pub struct IcalComposer {
    /// The configured command, spawned on the path of the iCalendar.
    pub command: CommandConfig,
}

impl IcalComposer {
    /// Edits `seed` in the composer and hands back what it wrote.
    ///
    /// The editor is the decision: an item that came back changed is the
    /// item, and one the composer emptied or handed back untouched is an
    /// edit given up on, which `None` reports. Nothing is asked after it,
    /// so a composer owning its own save and discard is not second-
    /// guessed by a menu, and a plain editor keeps the meaning its own
    /// quit already has.
    ///
    /// Nothing is written here: the draft comes back for the caller to
    /// write, which is what keeps the network out of the editor's way.
    pub fn edit(&self, printer: &mut impl Printer, seed: &[u8]) -> Result<Option<IcalDraft>> {
        // NOTE: the composer inherits this process' stdout, where it would
        // interleave with the JSON payload, and a consumer reading that
        // output has no terminal to edit in either.
        if printer.is_json() {
            bail!("Cannot run a composer under --json; pass the iCalendar explicitly instead");
        }

        let path = temp_dir().join(format!("unfog-{}.ics", uuid_v4()?));
        info!("seeding the composer with {} bytes in {path:?}", seed.len());
        fs::write(&path, seed).with_context(|| format!("Cannot write iCalendar {path:?}"))?;

        let mut draft = IcalDraft {
            path,
            contents: Vec::new(),
        };

        loop {
            let status = self.spawn(&draft.path)?;

            if !status.success() {
                // NOTE: a composer exiting non-zero is a person saying no,
                // so the file goes with it rather than piling up in the
                // temporary directory.
                draft.remove();
                bail!("Composer exited with {status}, leaving the item untouched");
            }

            let contents = fs::read(&draft.path)
                .with_context(|| format!("Cannot read iCalendar {:?}", draft.path))?;

            if contents.is_empty() {
                debug!("composer emptied the file");
                draft.remove();
                return Ok(None);
            }

            if contents == seed {
                // NOTE: a composer handing back the bytes it was given is
                // a person who quit without writing, which is the only
                // way a plain editor has of saying no.
                debug!("composer left the item untouched");
                draft.remove();
                return Ok(None);
            }

            draft.contents = contents;

            // NOTE: unfog checks what it is about to write, whichever
            // composer wrote it: a plain editor happily hands back a
            // VTODO missing its DTSTAMP, and something has to refuse it.
            let violations = check(&draft.contents);

            if !violations.is_empty() {
                println!("What the composer wrote is not a valid iCalendar:\n");

                for violation in &violations {
                    println!("  {violation}");
                }

                println!();

                match prompt::bool("Re-edit the item?", true) {
                    Ok(true) => continue,
                    Ok(false) => return Err(draft.keep(anyhow!(violations.join(", ")))),
                    Err(err) => return Err(draft.keep(err.into())),
                }
            }

            return Ok(Some(draft));
        }
    }

    /// Spawns the composer on `path`, inheriting every stream.
    fn spawn(&self, path: &Path) -> Result<ExitStatus> {
        let mut command = match &self.command {
            // NOTE: a shell invoked as `sh -c <line> <path>` binds the path
            // to `$0` rather than passing it on, so a shell composer takes
            // it interpolated into the line instead.
            CommandConfig::Shell(line) => {
                command::shell(&format!("{line} {}", quote(&path.to_string_lossy())))
            }
            CommandConfig::Argv { .. } => {
                let mut command = self.command.to_command();
                command.arg(path);
                command
            }
        };

        info!("spawning the composer on {path:?}");

        // NOTE: inherited is the default, and stating it is the point of
        // this module: capturing any of the three is what breaks an editor.
        command
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());

        command.status().context("Cannot spawn composer")
    }
}

/// A composed iCalendar, still in the file the composer wrote it to.
///
/// The file outlives the composer on purpose: a write that fails after a
/// minute of editing must not take the edit with it.
pub struct IcalDraft {
    /// The temporary file the composer edited.
    path: PathBuf,
    /// What it left there.
    pub contents: Vec<u8>,
}

impl IcalDraft {
    /// Settles the draft on the outcome of the write it fed.
    ///
    /// A write that landed drops the file, and one that failed keeps it
    /// and names it, so nothing typed is ever lost to a later failure.
    pub fn finish<T>(self, result: Result<T>) -> Result<T> {
        match result {
            Ok(value) => {
                self.remove();
                Ok(value)
            }
            Err(err) => Err(self.keep(err)),
        }
    }

    /// Keeps the file and names it in the error.
    ///
    /// The path is the recovery: the item is still there to be passed
    /// back in once whatever failed is dealt with.
    fn keep(&self, err: Error) -> Error {
        err.context(format!("Cannot edit iCalendar {:?}", self.path))
    }

    /// Drops the file, a failure to do so being nothing to report.
    fn remove(&self) {
        if let Err(err) = fs::remove_file(&self.path) {
            debug!("cannot remove {:?}: {err}", self.path);
        }
    }
}

/// Single-quotes a path for a shell command line.
fn quote(path: &str) -> String {
    format!("'{}'", path.replace('\'', r"'\''"))
}

#[cfg(test)]
mod tests {
    use super::quote;

    #[test]
    fn a_quoted_path_survives_a_quote_of_its_own() {
        assert_eq!(quote("/tmp/plain.ics"), "'/tmp/plain.ics'");
        assert_eq!(quote("/tmp/a b.ics"), "'/tmp/a b.ics'");
        assert_eq!(quote("/tmp/it's.ics"), r"'/tmp/it'\''s.ics'");
    }
}
