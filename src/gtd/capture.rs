//! # Capture
//!
//! The `capture` command: a VTODO carrying `CATEGORIES:inbox`, in the
//! capture tasklist, to clarify later.

use core::fmt;
use std::io::{IsTerminal, Read, stdin};

use anyhow::{Context, Result, bail};
use clap::Parser;
use jiff::Timestamp;
use mail_parser::MessageParser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::{
    arg::TasklistIdArg,
    client::TaskClient,
    ical::{blank_task, ensure_valid},
    task::{
        Task,
        fields::{TaskFieldsArgs, captured, revise},
        gtd::{GtdState, edit_categories},
    },
};

/// Capture a task into the Inbox.
///
/// Writes a task carrying the `inbox` category to the capture tasklist:
/// `-l`, else `capture.tasklist`, else `tasklist.default`, else the
/// tasklist the backend marks as the default one, else the only one.
/// With no text the standard input is read, so a script or a mail filter
/// can capture: a message (RFC 5322) gives its `Subject` as the summary
/// and its `Message-ID` as an `ATTACH:mid:` link back to it, any other
/// text its first line as the summary and the rest as the description.
/// A capture carries `CREATED`, the instant it was captured.
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
pub fn capture(client: TaskClient, flag: Option<String>, text: &str) -> Result<CaptureOutput> {
    capture_at(client, flag, text, Timestamp::now())
}

/// Captures `text` at `now`.
pub fn capture_at(
    mut client: TaskClient,
    flag: Option<String>,
    text: &str,
    now: Timestamp,
) -> Result<CaptureOutput> {
    let (summary, description, mid) = match message(text) {
        Some(message) => (message.subject, None, message.mid),
        None => {
            let (summary, description) = split(text);
            (summary.to_owned(), description.map(str::to_owned), None)
        }
    };
    let summary = summary.as_str();

    if summary.is_empty() {
        bail!("Nothing to capture; the text is blank");
    }

    let tasklist = capture_tasklist(&mut client, flag)?;

    let task = capture_body(summary, description, mid.as_deref(), now)?;

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

/// The VTODO a capture writes: a fresh `UID`, `DTSTAMP` and `CREATED` at
/// `now`, `summary`, `description` when given, `CATEGORIES:inbox`, and
/// for a message its `ATTACH:mid:`.
pub fn capture_body(
    summary: &str,
    description: Option<String>,
    mid: Option<&str>,
    now: Timestamp,
) -> Result<Vec<u8>> {
    let fields = TaskFieldsArgs {
        summary: Some(summary.to_owned()),
        description,
        ..Default::default()
    };

    let task = edit_categories(&fields.apply(&blank_task()?)?, Some(GtdState::Inbox), &[])?;
    ensure_valid(&task)?;
    captured(&revise(&task, now)?, now, mid)
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

/// A message piped to a capture.
struct Message {
    /// Its `Subject`, `(no subject)` when it states none.
    subject: String,
    /// Its `Message-ID`, bare of angle brackets.
    mid: Option<String>,
}

/// The message `text` is, when it opens on a header block (RFC 5322
/// 2.2: a field name, a colon) stating a `Subject` or a `Message-ID`.
fn message(text: &str) -> Option<Message> {
    let first = text.lines().next()?;
    let (name, _) = first.split_once(':')?;

    // NOTE: a field name is printable ASCII but the colon, no space, so
    // "Call Jo: the lease" stays a plain capture.
    if name.is_empty() || !name.bytes().all(|byte| byte.is_ascii_graphic()) {
        return None;
    }

    let parsed = MessageParser::default().parse_headers(text.as_bytes())?;
    let subject = parsed
        .subject()
        .map(str::trim)
        .filter(|subject| !subject.is_empty());
    let mid = parsed
        .message_id()
        .map(|mid| mid.trim().trim_start_matches('<').trim_end_matches('>'))
        .filter(|mid| !mid.is_empty());

    if subject.is_none() && mid.is_none() {
        return None;
    }

    Some(Message {
        subject: subject.unwrap_or("(no subject)").to_owned(),
        mid: mid.map(str::to_owned),
    })
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
    use crate::gtd::capture::{message, split};

    #[test]
    fn a_message_gives_its_subject_and_its_message_id() {
        let mail = "Return-Path: <jo@example.org>\r\nFrom: Jo <jo@example.org>\r\n\
                    Subject: =?UTF-8?Q?Caf=C3=A9_on_Friday?=\r\n\
                    Message-ID: <abc.123@example.org>\r\n\r\nSee you there.\r\n";
        let parsed = message(mail).unwrap();
        assert_eq!(parsed.subject, "Café on Friday");
        assert_eq!(parsed.mid.as_deref(), Some("abc.123@example.org"));

        assert!(message("Call Jo: about the lease\nsoon").is_none());
        assert!(message("Note:buy milk").is_none());
    }

    #[test]
    fn the_first_line_is_the_summary_and_the_rest_the_description() {
        assert_eq!(split("  Buy milk \n"), ("Buy milk", None));
        assert_eq!(
            split("Call Jo\r\n\r\nabout the lease\n"),
            ("Call Jo", Some("about the lease"))
        );
    }
}
