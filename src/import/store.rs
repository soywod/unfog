//! # 1.x store
//!
//! The event log unfog 1.x kept its tasks in, one Haskell `Show` value
//! per line (lib/Unfog/Event/Type.hs), and the older v0 format it
//! migrated from (lib/Unfog/Event/TypeV0.hs).
//!
//! The reader is a small recursive one over the few shapes `Show` writes
//! for those types: a constructor name, a `UTCTime`, a string with its
//! Haskell escapes, a `Maybe` in parentheses, an `Int`, and a list of
//! strings.

use anyhow::{Context, Result, anyhow, bail};
use jiff::{Timestamp, civil::DateTime, tz::TimeZone};
use log::warn;

use crate::shared::uuid::uuid_v4;

/// One event of the 1.x log, the v0 ones mapped onto it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportEvent {
    /// A task added: its id, description, project and due date.
    Added {
        at: Timestamp,
        id: String,
        desc: String,
        project: Option<String>,
        due: Option<Timestamp>,
    },
    /// A task edited: the same fields, all of them replaced.
    Edited {
        at: Timestamp,
        id: String,
        desc: String,
        project: Option<String>,
        due: Option<Timestamp>,
    },
    /// Work on a task started.
    Started { at: Timestamp, id: String },
    /// Work on a task stopped.
    Stopped { at: Timestamp, id: String },
    /// A task done.
    Did { at: Timestamp, id: String },
    /// A task done no more.
    Undid { at: Timestamp, id: String },
    /// A task deleted.
    Deleted { at: Timestamp, id: String },
    /// A task deleted no more.
    Undeleted { at: Timestamp, id: String },
    /// The context changed, which names no task.
    Context,
}

/// The format a store was read in.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ImportFormat {
    /// The 1.x log.
    V1,
    /// The v0 log 1.x migrated from.
    V0,
}

/// Reads a whole store: in the 1.x format, or when one of its lines does
/// not read in it, in the v0 format, as 1.x itself falls back. Blank
/// lines are skipped.
///
/// v0 events address tasks by a number, each given a fresh UUID at its
/// creation, and are put in time order as 1.x's migration does; an event
/// naming a number no creation introduced is dropped.
pub fn read_store(contents: &str) -> Result<(ImportFormat, Vec<ImportEvent>)> {
    let lines: Vec<(usize, &str)> = contents
        .lines()
        .enumerate()
        .map(|(index, line)| (index + 1, line.trim()))
        .filter(|(_, line)| !line.is_empty())
        .collect();

    let v1: Result<Vec<ImportEvent>> = lines
        .iter()
        .map(|(number, line)| read_v1(line).with_context(|| format!("Read line {number}")))
        .collect();

    let v1_err = match v1 {
        Ok(events) => return Ok((ImportFormat::V1, events)),
        Err(err) => err,
    };

    let v0: Result<Vec<V0Event>> = lines
        .iter()
        .map(|(number, line)| read_v0(line).with_context(|| format!("Read line {number}")))
        .collect();

    match v0 {
        Ok(events) => Ok((ImportFormat::V0, migrate(events)?)),
        Err(_) => Err(v1_err.context("Read the store as a 1.x or a v0 event log")),
    }
}

/// Reads one 1.x event.
fn read_v1(line: &str) -> Result<ImportEvent> {
    let mut cursor = Cursor::new(line);
    let name = cursor.ident()?;

    let event = match name {
        "TaskAdded" | "TaskEdited" => {
            let at = cursor.time()?;
            let id = cursor.string()?;
            let desc = cursor.string()?;
            let project = cursor.maybe(Cursor::string)?;
            let due = cursor.maybe(Cursor::time)?;

            match name {
                "TaskAdded" => ImportEvent::Added {
                    at,
                    id,
                    desc,
                    project,
                    due,
                },
                _ => ImportEvent::Edited {
                    at,
                    id,
                    desc,
                    project,
                    due,
                },
            }
        }
        "TaskStarted" | "TaskStopped" | "TaskDid" | "TaskUndid" | "TaskDeleted"
        | "TaskUndeleted" => {
            let at = cursor.time()?;
            let id = cursor.string()?;

            match name {
                "TaskStarted" => ImportEvent::Started { at, id },
                "TaskStopped" => ImportEvent::Stopped { at, id },
                "TaskDid" => ImportEvent::Did { at, id },
                "TaskUndid" => ImportEvent::Undid { at, id },
                "TaskDeleted" => ImportEvent::Deleted { at, id },
                _ => ImportEvent::Undeleted { at, id },
            }
        }
        "ContextEdited" => {
            cursor.time()?;
            cursor.maybe(Cursor::string)?;
            ImportEvent::Context
        }
        name => bail!("Unknown 1.x event `{name}`"),
    };

    cursor.end()?;
    Ok(event)
}

/// One v0 event, its task named by a number.
#[derive(Clone, Debug)]
enum V0Event {
    /// `TaskCreated` or `TaskUpdated`: the number, the description, the
    /// tags and the due date.
    Written {
        created: bool,
        at: Timestamp,
        number: i64,
        desc: String,
        tags: Vec<String>,
        due: Option<Timestamp>,
    },
    /// Any event naming a task and nothing else, with the 1.x event it
    /// maps onto.
    Marked {
        at: Timestamp,
        number: i64,
        to: fn(Timestamp, String) -> ImportEvent,
    },
    /// `ContextSet`, which names no task.
    Context { at: Timestamp },
}

/// Reads one v0 event.
fn read_v0(line: &str) -> Result<V0Event> {
    let mut cursor = Cursor::new(line);
    let name = cursor.ident()?;

    let mark = |to: fn(Timestamp, String) -> ImportEvent, cursor: &mut Cursor<'_>| {
        let at = cursor.time()?;
        let number = cursor.int()?;
        cursor.int()?;
        Ok::<_, anyhow::Error>(V0Event::Marked { at, number, to })
    };

    let event = match name {
        "TaskCreated" | "TaskUpdated" => {
            let at = cursor.time()?;
            let number = cursor.int()?;
            cursor.int()?;
            cursor.int()?;
            let desc = cursor.string()?;
            let tags = cursor.strings()?;
            let due = cursor.maybe(Cursor::time)?;

            V0Event::Written {
                created: name == "TaskCreated",
                at,
                number,
                desc,
                tags,
                due,
            }
        }
        "TaskStarted" => mark(|at, id| ImportEvent::Started { at, id }, &mut cursor)?,
        "TaskStopped" => mark(|at, id| ImportEvent::Stopped { at, id }, &mut cursor)?,
        "TaskMarkedAsDone" => mark(|at, id| ImportEvent::Did { at, id }, &mut cursor)?,
        "TaskUnmarkedAsDone" => mark(|at, id| ImportEvent::Undid { at, id }, &mut cursor)?,
        "TaskDeleted" => mark(|at, id| ImportEvent::Deleted { at, id }, &mut cursor)?,
        "ContextSet" => {
            let at = cursor.time()?;
            cursor.strings()?;
            V0Event::Context { at }
        }
        name => bail!("Unknown v0 event `{name}`"),
    };

    cursor.end()?;
    Ok(event)
}

/// Maps v0 events onto 1.x ones, as lib/Unfog/Event/MigrationV0.hs does:
/// a fresh UUID per created number, the tags joined by `-` into a
/// project, the events put in time order.
fn migrate(events: Vec<V0Event>) -> Result<Vec<ImportEvent>> {
    let mut ids: Vec<(i64, String)> = Vec::new();
    let mut timed: Vec<(Timestamp, ImportEvent)> = Vec::new();

    for event in events {
        match event {
            V0Event::Written {
                created,
                at,
                number,
                desc,
                tags,
                due,
            } => {
                let project = match tags.is_empty() {
                    true => None,
                    false => Some(tags.join("-")),
                };

                if created {
                    let id = uuid_v4()?;
                    ids.push((number, id.clone()));
                    timed.push((
                        at,
                        ImportEvent::Added {
                            at,
                            id,
                            desc,
                            project,
                            due,
                        },
                    ));
                } else if let Some(id) = id_of(&ids, number) {
                    timed.push((
                        at,
                        ImportEvent::Edited {
                            at,
                            id,
                            desc,
                            project,
                            due,
                        },
                    ));
                } else {
                    warn!("skipping a v0 update of task {number}, never created");
                }
            }
            V0Event::Marked { at, number, to } => match id_of(&ids, number) {
                Some(id) => timed.push((at, to(at, id))),
                None => warn!("skipping a v0 event of task {number}, never created"),
            },
            V0Event::Context { at } => timed.push((at, ImportEvent::Context)),
        }
    }

    timed.sort_by_key(|(at, _)| *at);

    Ok(timed.into_iter().map(|(_, event)| event).collect())
}

/// The UUID a v0 number was given at its creation.
fn id_of(ids: &[(i64, String)], number: i64) -> Option<String> {
    ids.iter()
        .find(|(known, _)| *known == number)
        .map(|(_, id)| id.clone())
}

/// A position in one line of `Show` output.
struct Cursor<'a> {
    line: &'a str,
    at: usize,
}

impl<'a> Cursor<'a> {
    fn new(line: &'a str) -> Self {
        Self { line, at: 0 }
    }

    /// What is left of the line.
    fn rest(&self) -> &'a str {
        &self.line[self.at..]
    }

    /// Skips spaces.
    fn spaces(&mut self) {
        let rest = self.rest();
        self.at += rest.len() - rest.trim_start().len();
    }

    /// Consumes `token`, failing when the line does not go on with it.
    fn expect(&mut self, token: &str) -> Result<()> {
        self.spaces();

        match self.rest().strip_prefix(token) {
            Some(_) => {
                self.at += token.len();
                Ok(())
            }
            None => bail!("Expected `{token}` at `{}`", self.rest()),
        }
    }

    /// Consumes `token` when the line goes on with it.
    fn eat(&mut self, token: &str) -> bool {
        self.spaces();

        match self.rest().starts_with(token) {
            true => {
                self.at += token.len();
                true
            }
            false => false,
        }
    }

    /// Fails unless nothing but spaces is left.
    fn end(&mut self) -> Result<()> {
        self.spaces();

        match self.rest().is_empty() {
            true => Ok(()),
            false => bail!("Unexpected `{}` at the end of the line", self.rest()),
        }
    }

    /// A run of characters none of which is a space or a delimiter.
    fn word(&mut self) -> &'a str {
        self.spaces();

        let rest = self.rest();
        let len = rest
            .find(|c: char| c.is_whitespace() || matches!(c, '(' | ')' | '[' | ']' | ',' | '"'))
            .unwrap_or(rest.len());

        self.at += len;
        &rest[..len]
    }

    /// A constructor name.
    fn ident(&mut self) -> Result<&'a str> {
        let word = self.word();

        if !word.starts_with(|c: char| c.is_ascii_uppercase()) {
            bail!("Expected a constructor, found `{word}`");
        }

        Ok(word)
    }

    /// An `Int`, negative ones parenthesized as `Show` writes them.
    fn int(&mut self) -> Result<i64> {
        if self.eat("(") {
            let value = self.int()?;
            self.expect(")")?;
            return Ok(value);
        }

        let word = self.word();
        word.parse()
            .map_err(|_| anyhow!("Expected a number, found `{word}`"))
    }

    /// A `UTCTime`: `YYYY-MM-DD HH:MM:SS[.fraction] UTC`.
    fn time(&mut self) -> Result<Timestamp> {
        let day = self.word();
        let clock = self.word();
        let zone = self.word();

        if zone != "UTC" {
            bail!("Expected a UTC time, found `{day} {clock} {zone}`");
        }

        let numbers = |text: &str, separator: char| -> Result<Vec<i64>> {
            text.split(separator)
                .map(|part| part.parse::<i64>().map_err(Into::into))
                .collect()
        };

        let (whole, fraction) = clock.split_once('.').unwrap_or((clock, ""));

        let [year, month, day] = numbers(day, '-')?[..] else {
            bail!("Expected a date, found `{day}`");
        };

        let [hour, minute, second] = numbers(whole, ':')?[..] else {
            bail!("Expected a time, found `{clock}`");
        };

        let digits: String = fraction
            .chars()
            .chain("000000000".chars())
            .take(9)
            .collect();
        let nanos: i32 = match fraction.is_empty() {
            true => 0,
            false => digits.parse()?,
        };

        let civil = DateTime::new(
            i16::try_from(year)?,
            i8::try_from(month)?,
            i8::try_from(day)?,
            i8::try_from(hour)?,
            i8::try_from(minute)?,
            i8::try_from(second)?,
            nanos,
        )?;

        Ok(civil.to_zoned(TimeZone::UTC)?.timestamp())
    }

    /// `Nothing`, or `(Just <value>)`.
    fn maybe<T>(&mut self, value: impl FnOnce(&mut Self) -> Result<T>) -> Result<Option<T>> {
        if self.eat("Nothing") {
            return Ok(None);
        }

        self.expect("(")?;
        self.expect("Just")?;
        let value = value(self)?;
        self.expect(")")?;

        Ok(Some(value))
    }

    /// A list of strings: `["a","b"]`.
    fn strings(&mut self) -> Result<Vec<String>> {
        self.expect("[")?;

        let mut strings = Vec::new();

        if self.eat("]") {
            return Ok(strings);
        }

        loop {
            strings.push(self.string()?);

            if self.eat("]") {
                return Ok(strings);
            }

            self.expect(",")?;
        }
    }

    /// A string literal, its Haskell escapes resolved.
    fn string(&mut self) -> Result<String> {
        self.expect("\"")?;

        let mut text = String::new();
        let mut chars = self.rest().char_indices().peekable();

        while let Some((at, c)) = chars.next() {
            match c {
                '"' => {
                    self.at += at + 1;
                    return Ok(text);
                }
                '\\' => {
                    let rest = &self.rest()[at + 1..];
                    let (escaped, len) = escape(rest)?;
                    text.extend(escaped);

                    for _ in 0..rest[..len].chars().count() {
                        chars.next();
                    }
                }
                c => text.push(c),
            }
        }

        bail!("Unterminated string")
    }
}

/// The ASCII control names Haskell escapes, longest first so `SOH` wins
/// over `SO`.
const ASCII_NAMES: [(&str, char); 34] = [
    ("NUL", '\x00'),
    ("SOH", '\x01'),
    ("STX", '\x02'),
    ("ETX", '\x03'),
    ("EOT", '\x04'),
    ("ENQ", '\x05'),
    ("ACK", '\x06'),
    ("BEL", '\x07'),
    ("DLE", '\x10'),
    ("DC1", '\x11'),
    ("DC2", '\x12'),
    ("DC3", '\x13'),
    ("DC4", '\x14'),
    ("NAK", '\x15'),
    ("SYN", '\x16'),
    ("ETB", '\x17'),
    ("CAN", '\x18'),
    ("SUB", '\x1a'),
    ("ESC", '\x1b'),
    ("DEL", '\x7f'),
    ("BS", '\x08'),
    ("HT", '\x09'),
    ("LF", '\x0a'),
    ("VT", '\x0b'),
    ("FF", '\x0c'),
    ("CR", '\x0d'),
    ("SO", '\x0e'),
    ("SI", '\x0f'),
    ("EM", '\x19'),
    ("FS", '\x1c'),
    ("GS", '\x1d'),
    ("RS", '\x1e'),
    ("US", '\x1f'),
    ("SP", ' '),
];

/// Resolves one escape, `rest` following its backslash: the character it
/// stands for (none for `\&`) and how many bytes it spans.
fn escape(rest: &str) -> Result<(Option<char>, usize)> {
    let Some(first) = rest.chars().next() else {
        bail!("Unterminated escape");
    };

    let simple = match first {
        'n' => Some('\n'),
        't' => Some('\t'),
        'r' => Some('\r'),
        'a' => Some('\x07'),
        'b' => Some('\x08'),
        'f' => Some('\x0c'),
        'v' => Some('\x0b'),
        '\\' => Some('\\'),
        '"' => Some('"'),
        '\'' => Some('\''),
        _ => None,
    };

    if let Some(c) = simple {
        return Ok((Some(c), 1));
    }

    if first == '&' {
        return Ok((None, 1));
    }

    if first == '^' {
        let control = rest[1..]
            .chars()
            .next()
            .filter(|c| ('@'..='_').contains(c))
            .ok_or_else(|| anyhow!("Invalid control escape"))?;
        let code = control as u32 - '@' as u32;
        return Ok((char::from_u32(code), 2));
    }

    let (radix, digits_at) = match first {
        'x' => (16, 1),
        'o' => (8, 1),
        c if c.is_ascii_digit() => (10, 0),
        _ => {
            for (name, c) in ASCII_NAMES {
                if rest.starts_with(name) {
                    return Ok((Some(c), name.len()));
                }
            }

            bail!("Unknown escape `\\{first}`");
        }
    };

    let digits: String = rest[digits_at..]
        .chars()
        .take_while(|c| c.is_digit(radix))
        .collect();

    let code = u32::from_str_radix(&digits, radix)?;
    let c = char::from_u32(code).ok_or_else(|| anyhow!("Invalid character code {code}"))?;

    Ok((Some(c), digits_at + digits.len()))
}

#[cfg(test)]
mod tests {
    use crate::import::store::{Cursor, ImportEvent, ImportFormat, read_store, read_v0, read_v1};

    fn at(stamp: &str) -> jiff::Timestamp {
        stamp.parse().unwrap()
    }

    #[test]
    fn a_v1_event_reads_with_its_maybes() {
        let line = "TaskAdded 2020-12-01 09:00:00 UTC \"id-1\" \"Write\" (Just \"work\") \
                    (Just 2020-12-10 17:00:00 UTC)";

        assert_eq!(
            read_v1(line).unwrap(),
            ImportEvent::Added {
                at: at("2020-12-01T09:00:00Z"),
                id: "id-1".into(),
                desc: "Write".into(),
                project: Some("work".into()),
                due: Some(at("2020-12-10T17:00:00Z")),
            }
        );
    }

    #[test]
    fn a_time_keeps_its_fraction() {
        let mut cursor = Cursor::new("2020-12-01 09:05:30.25 UTC");
        assert_eq!(cursor.time().unwrap(), at("2020-12-01T09:05:30.25Z"));
    }

    #[test]
    fn a_string_resolves_every_escape_show_writes() {
        let mut cursor = Cursor::new(r#""Caf\233 \"x\"\n\1234\&5\SOH\SO\&H\^A\\""#);

        assert_eq!(
            cursor.string().unwrap(),
            "Caf\u{e9} \"x\"\n\u{4d2}5\x01\x0eH\x01\\"
        );
    }

    #[test]
    fn a_v0_event_reads_its_numbers_and_tags() {
        assert!(
            read_v0("TaskCreated 2019-06-01 08:00:00 UTC 1 1 0 \"Buy\" [\"a\",\"b\"] Nothing")
                .is_ok()
        );
        assert!(read_v0("TaskMarkedAsDone 2019-06-01 10:00:00 UTC 1 (-1)").is_ok());
        assert!(read_v0("ContextSet 2019-06-01 10:05:00 UTC []").is_ok());
    }

    #[test]
    fn a_store_falls_back_to_v0_and_numbers_get_uuids() {
        let store = include_str!("../../tests/fixtures/store-0.x");
        let (format, events) = read_store(store).unwrap();

        assert_eq!(format, ImportFormat::V0);

        let ImportEvent::Added { id, project, .. } = &events[0] else {
            panic!("{events:?}");
        };
        assert_eq!(id.len(), 36);
        assert_eq!(project.as_deref(), Some("home-shop"));
    }

    #[test]
    fn a_line_neither_format_reads_is_refused_by_number() {
        let err = read_store("TaskAdded nonsense\n").unwrap_err();
        assert!(format!("{err:#}").contains("line 1"), "{err:#}");
    }
}
