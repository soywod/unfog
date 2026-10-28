//! # iCalendar source
//!
//! Where the task commands get raw iCalendar bytes: a file, the standard
//! input, the argument itself, or nothing at all.
//!
//! It also holds the two halves of the strict reading unfog keeps for the
//! bytes it wrote itself: [`check`], which weighs an item against the RFC
//! 5545 contract, and [`blank_task`], which mints the identity a composer
//! cannot invent.

use std::{
    fs,
    io::{Read, stdin},
    path::PathBuf,
};

use anyhow::{Context, Result, bail};
use chrono::Utc;
use ical::{
    component::{IcalComponent, IcalComponentKind, IcalComponentName},
    ical::Ical,
    prop::{IcalProp, IcalPropKind, IcalPropName},
    tree::cst::IcalCst,
    value::{IcalValue, datetime::IcalDateTime, text::IcalText},
    version::IcalVersion,
};

use crate::shared::uuid::uuid_v4;

/// `PRODID` a minted item is stamped with (RFC 5545 3.7.3).
pub const PRODID: &str = "-//Pimalaya//unfog//EN";

/// Resolves an iCalendar source into raw bytes.
///
/// `-` reads stdin and an existing path is read from disk, otherwise the
/// value is taken as literal iCalendar contents.
///
/// A source carrying nothing but whitespace is refused rather than passed
/// on: handing a backend an empty body is never what was meant.
pub fn read_source(source: &str) -> Result<Vec<u8>> {
    if source == "-" {
        let mut buf = Vec::new();
        stdin()
            .read_to_end(&mut buf)
            .context("Read iCalendar from stdin error")?;

        return refuse_blank(buf, "stdin");
    }

    let path = PathBuf::from(source);

    if path.is_file() {
        let contents = fs::read(&path)
            .with_context(|| format!("Read iCalendar from `{}` error", path.display()))?;

        return refuse_blank(contents, &format!("`{}`", path.display()));
    }

    let trimmed = source.trim_start();

    if trimmed.starts_with("BEGIN:VCALENDAR") || trimmed.starts_with("BEGIN:V") {
        return Ok(source.as_bytes().to_vec());
    }

    bail!("Source `{source}` is neither a readable file nor iCalendar contents")
}

/// What is wrong with an item, empty when nothing is.
///
/// Checks it against the RFC 5545 contract through ical-rs, so a
/// VCALENDAR missing its PRODID or a VTODO missing its DTSTAMP is caught
/// here rather than by the server, or worse by neither. Reading is
/// liberal and this is the strict half: bytes that do not parse at all
/// come back as the one violation they are.
pub fn check(item: &[u8]) -> Vec<String> {
    let cst = match IcalCst::parse(item) {
        Ok(cst) => cst,
        Err(err) => return vec![format!("{err}")],
    };

    match cst.decode().validate() {
        Ok(_) => Vec::new(),
        Err(errors) => errors.iter().map(|err| format!("{err}")).collect(),
    }
}

/// Refuses an item that does not pass [`check`], naming every violation.
///
/// What a create made from flags alone is held to: the item is unfog's
/// own then, and has to be one a server takes.
pub fn ensure_valid(item: &[u8]) -> Result<()> {
    let violations = check(item);

    if !violations.is_empty() {
        bail!(
            "The task these flags describe is not a valid iCalendar:\n\n  {}\n",
            violations.join("\n  ")
        );
    }

    Ok(())
}

/// Whether an item carries a component of `kind`.
///
/// Only the two backends that narrow a listing after parsing it read
/// this; CalDAV pushes the kind down.
///
/// Bytes that do not parse carry nothing, which is what a projection
/// makes of them too.
#[cfg(any(feature = "vdir", feature = "pimdir"))]
pub fn holds_kind(contents: &[u8], kind: IcalComponentKind) -> bool {
    let Ok(cst) = IcalCst::parse(contents) else {
        return false;
    };

    cst.decode()
        .components
        .iter()
        .any(|component| component.name.eq_ignore_ascii_case(&kind))
}

/// Mints a VTODO carrying nothing but its identity and what it owes.
///
/// This is what a create starts from when it is given no source, and it
/// is an item rather than an empty file so that no composer is asked to
/// invent a `UID`: a backend may key the item on it, and an editor
/// handed an empty file mints none.
pub fn blank_task() -> Result<Vec<u8>> {
    let now = Utc::now().format("%Y%m%dT%H%M%SZ").to_string();

    let vtodo = IcalComponent {
        name: IcalComponentName::Kind(IcalComponentKind::VTodo),
        props: vec![
            text_prop(IcalPropKind::Uid, uuid_v4()?),
            stamp_prop(IcalPropKind::DtStamp, &now),
        ],
        components: Vec::new(),
    };

    Ok(IcalCst::from(calendar(vec![vtodo])).to_bytes())
}

/// A VCALENDAR carrying `components`, stamped with unfog's `PRODID`.
pub fn calendar(components: Vec<IcalComponent<'static>>) -> Ical<'static> {
    Ical {
        version: IcalVersion::V2_0,
        props: vec![text_prop(IcalPropKind::ProdId, PRODID)],
        components,
    }
}

/// Refuses a source that carries no item at all.
fn refuse_blank(contents: Vec<u8>, source: &str) -> Result<Vec<u8>> {
    if contents.iter().all(u8::is_ascii_whitespace) {
        bail!("No iCalendar to read from {source}");
    }

    Ok(contents)
}

/// A text property under a canonical name, carrying no parameter.
pub fn text_prop(kind: IcalPropKind, value: impl ToString) -> IcalProp<'static> {
    prop(kind, IcalValue::Text(IcalText(value.to_string().into())))
}

/// A date-time property under a canonical name, carrying no parameter.
pub fn stamp_prop(kind: IcalPropKind, value: impl ToString) -> IcalProp<'static> {
    prop(
        kind,
        IcalValue::DateTime(IcalDateTime(value.to_string().into())),
    )
}

/// A property under a canonical name, carrying no parameter.
fn prop(kind: IcalPropKind, value: IcalValue<'static>) -> IcalProp<'static> {
    IcalProp {
        name: IcalPropName::Kind(kind),
        params: Vec::new(),
        value,
    }
}

#[cfg(test)]
mod tests {
    #[cfg(any(feature = "vdir", feature = "pimdir"))]
    use ical::component::IcalComponentKind;

    #[cfg(any(feature = "vdir", feature = "pimdir"))]
    use crate::shared::ical::holds_kind;
    use crate::shared::ical::{blank_task, check, read_source};

    #[test]
    fn a_minted_task_passes_the_check_it_will_be_held_to() {
        let item = blank_task().unwrap();
        let text = String::from_utf8(item.clone()).unwrap();

        assert!(text.contains("BEGIN:VTODO\r\n"), "{text}");
        assert!(text.contains("PRODID:-//Pimalaya//unfog//EN\r\n"), "{text}");
        assert!(text.contains("UID:"), "{text}");
        assert!(text.contains("DTSTAMP:"), "{text}");
        assert!(check(&item).is_empty(), "{:?}", check(&item));
    }

    #[cfg(any(feature = "vdir", feature = "pimdir"))]
    #[test]
    fn an_item_answers_the_kind_it_carries_and_no_other() {
        let item = |kind: &str| {
            format!(
                "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\n\
                 BEGIN:{kind}\r\nUID:a\r\nDTSTAMP:20260101T000000Z\r\n\
                 DTSTART:20260101T000000Z\r\nEND:{kind}\r\nEND:VCALENDAR\r\n"
            )
        };

        assert!(holds_kind(
            item("VTODO").as_bytes(),
            IcalComponentKind::VTodo
        ));
        assert!(!holds_kind(
            item("VTODO").as_bytes(),
            IcalComponentKind::VEvent
        ));
        assert!(!holds_kind(b"not an ical", IcalComponentKind::VTodo));
    }

    #[test]
    fn a_source_that_is_neither_a_file_nor_an_item_is_refused() {
        let err = read_source("not an ical").unwrap_err().to_string();

        assert!(err.contains("neither a readable file"), "{err}");
    }

    #[test]
    fn bytes_that_do_not_parse_come_back_as_one_violation() {
        assert_eq!(check(b"not an ical at all").len(), 1);
    }

    #[test]
    fn a_calendar_missing_its_prodid_is_caught_before_the_server_sees_it() {
        let item = b"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:a\r\n\
                     DTSTAMP:20260101T000000Z\r\nEND:VTODO\r\nEND:VCALENDAR\r\n";

        assert!(!check(item).is_empty());
    }
}
