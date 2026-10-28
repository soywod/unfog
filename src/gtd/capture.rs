//! # Capture
//!
//! The `capture` command: a VTODO carrying `CATEGORIES:inbox`, in the
//! capture tasklist, to clarify later.

use core::fmt;
use std::io::{IsTerminal, Read, stdin};

use anyhow::{Context, Result, bail};
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{
    arg::TasklistIdArg,
    client::TaskClient,
    ical::{blank_task, ensure_valid},
    task::{
        Task,
        fields::{TaskFieldsArgs, revise},
        gtd::GtdState,
    },
};

/// Capture a task into the Inbox.
///
/// Writes a task carrying the `inbox` category to the capture tasklist:
/// `-l`, else `capture.tasklist`, else `tasklist.default`, else the
/// tasklist the backend marks as the default one, else the only one.
/// With no text the standard input is read, so a script or a mail filter
/// can capture: its first line is the summary, the rest the
/// description.
///
/// JSON output: `{"id", "uid", "tasklist", "summary"}`.
#[derive(Debug, Parser)]
pub struct CaptureCommand {
    #[command(flatten)]
    pub tasklist: TasklistIdArg,
    /// What to capture, the words joined; omit to read stdin.
    #[arg(value_name = "TEXT", trailing_var_arg = true)]
    pub text: Vec<String>,
}

impl CaptureCommand {
    pub fn execute(self, printer: &mut impl Printer, client: TaskClient) -> Result<()> {
        let text = match self.text.is_empty() {
            false => self.text.join(" "),
            true if stdin().is_terminal() => {
                bail!("Nothing to capture; give a text, or pipe one to stdin")
            }
            true => {
                let mut text = String::new();
                stdin()
                    .read_to_string(&mut text)
                    .context("Read the capture from stdin error")?;
                text
            }
        };

        printer.out(capture(client, self.tasklist.id, &text)?)
    }
}

/// Captures `text` into the capture tasklist of `client`'s account, `flag`
/// naming one first.
pub fn capture(mut client: TaskClient, flag: Option<String>, text: &str) -> Result<CaptureOutput> {
    let (summary, description) = split(text);

    if summary.is_empty() {
        bail!("Nothing to capture; the text is blank");
    }

    let tasklist = capture_tasklist(&mut client, flag)?;

    let fields = TaskFieldsArgs {
        summary: Some(summary.to_owned()),
        description: description.map(str::to_owned),
        categories: vec![GtdState::Inbox.as_str().to_owned()],
        ..Default::default()
    };

    let task = fields.apply(&blank_task()?)?;
    ensure_valid(&task)?;
    let task = revise(&task, Timestamp::now())?;

    let uid = Task::project(&crate::shared::item::TaskItem {
        contents: task.clone(),
        ..Default::default()
    })
    .first()
    .map(|task| task.uid.clone())
    .unwrap_or_default();

    let id = client.create_item(&tasklist, task)?;

    Ok(CaptureOutput {
        id,
        uid,
        tasklist,
        summary: summary.to_owned(),
    })
}

/// The tasklist a capture goes to: the flag, `capture.tasklist`,
/// `tasklist.default`, the one the backend marks as the default, the only
/// one, in that order.
fn capture_tasklist(client: &mut TaskClient, flag: Option<String>) -> Result<String> {
    if let Some(id) = flag
        .or_else(|| client.account.capture_tasklist.clone())
        .or_else(|| client.account.tasklist_default.clone())
    {
        return Ok(id);
    }

    let tasklists = client.list_tasklists()?;

    if let Some(tasklist) = tasklists.iter().find(|tasklist| tasklist.default) {
        return Ok(tasklist.id.clone());
    }

    match tasklists.as_slice() {
        [tasklist] => Ok(tasklist.id.clone()),
        [] => bail!("No tasklist to capture to; create one first"),
        _ => bail!("Several tasklists to capture to; pass -l/--tasklist or set capture.tasklist"),
    }
}

/// A capture's summary, its first non-blank line, and its description,
/// the rest when there is any.
fn split(text: &str) -> (&str, Option<&str>) {
    let text = text.trim();

    match text.split_once('\n') {
        None => (text, None),
        Some((summary, rest)) => {
            let rest = rest.trim();
            (summary.trim(), (!rest.is_empty()).then_some(rest))
        }
    }
}

/// The task a capture wrote.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct CaptureOutput {
    /// Backend-assigned identifier of the new task.
    pub id: String,
    /// The new task's `UID`.
    pub uid: String,
    /// The tasklist it was written to.
    pub tasklist: String,
    /// Its summary.
    pub summary: String,
}

impl fmt::Display for CaptureOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Captured `{}` into the Inbox of tasklist `{}`",
            self.summary, self.tasklist
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::gtd::capture::split;

    #[test]
    fn the_first_line_is_the_summary_and_the_rest_the_description() {
        assert_eq!(split("  Buy milk \n"), ("Buy milk", None));
        assert_eq!(
            split("Call Jo\r\n\r\nabout the lease\n"),
            ("Call Jo", Some("about the lease"))
        );
    }
}
