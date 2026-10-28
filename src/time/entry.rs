//! # Time entries
//!
//! The two shapes a time entry takes (pimdir GUIDE.md §18), how they are
//! read off stored items and how they are written. Both are tied to the
//! task by `RELATED-TO` its `UID`, the default `PARENT` relation:
//!
//! - an event, a VEVENT with `DTSTART`, `DTEND` once stopped, the task's
//!   `SUMMARY` and `TRANSP:TRANSPARENT`, so it never reads as busy;
//! - a journal, a pair of VJOURNALs, `DTSTART` the instant and
//!   `CATEGORIES:start` or `stop`, as unfog 1.x logged it.

use anyhow::{Context, Result};
use ical::{
    component::{IcalComponent, IcalComponentKind, IcalComponentName, vevent::VEVENT},
    param::IcalParam,
    prop::{IcalProp, IcalPropKind},
    tree::{
        codec::mode::Escaper,
        cst::{IcalCst, IcalItem},
    },
    value::{IcalValue, text::IcalTextList},
};
use jiff::{Timestamp, civil::DateTime, tz::TimeZone};

use crate::shared::{
    ical::calendar,
    task::fields::{names, prop, text, utc_stamp},
};

/// The shape of a time entry.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Shape {
    /// One VEVENT, its `DTEND` the stop.
    Event,
    /// A VJOURNAL for the start and one for the stop.
    Journal,
}

/// A stored item, as a time place lists it.
#[derive(Clone, Debug)]
pub struct RawItem {
    /// The handle the place addresses it by.
    pub id: String,
    /// Its version, for the places that keep one.
    pub etag: Option<String>,
    /// Its bytes.
    pub contents: Vec<u8>,
}

/// One span of work on a task.
#[derive(Clone, Debug)]
pub struct TimeEntry {
    /// The entry's `UID`, the start's for a journal pair.
    pub uid: String,
    /// The `UID` of the task.
    pub task: String,
    /// The `SUMMARY` it was written with, the task's at the time.
    pub summary: String,
    /// When the work started.
    pub start: Timestamp,
    /// When it stopped, `None` while it runs.
    pub end: Option<Timestamp>,
    /// Its shape.
    pub shape: Shape,
    /// The items holding it: the event, or the start then the stop.
    pub items: Vec<RawItem>,
}

/// One VJOURNAL of a pair.
struct Mark {
    uid: String,
    task: String,
    summary: String,
    at: Timestamp,
    start: bool,
    item: RawItem,
}

/// The time entries `items` hold, in no particular order.
///
/// A VEVENT is an entry when it relates to a task. A VJOURNAL is half of
/// one when it relates to a task and carries `start` or `stop` (any
/// case): the marks of a task pair in time order, a start left without a
/// stop running, a start meeting another start ending at it, and a stop
/// with no start passed over. Every other item is not a time entry.
pub fn parse(items: Vec<RawItem>) -> Vec<TimeEntry> {
    let mut entries = Vec::new();
    let mut marks = Vec::new();

    for item in items {
        let Ok(cst) = IcalCst::parse(&item.contents) else {
            continue;
        };
        let ical = cst.decode();

        let Some(component) = ical.components.iter().find(|component| {
            component
                .name
                .eq_ignore_ascii_case(&IcalComponentKind::VEvent)
                || component
                    .name
                    .eq_ignore_ascii_case(&IcalComponentKind::VJournal)
        }) else {
            continue;
        };

        let Some(task) = value(component, "RELATED-TO") else {
            continue;
        };
        let Some(start) = component
            .props
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case("DTSTART"))
        else {
            continue;
        };
        let Some(start) = instant(start) else {
            continue;
        };

        let uid = value(component, "UID").unwrap_or_default();
        let summary = value(component, "SUMMARY").unwrap_or_default();

        if component
            .name
            .eq_ignore_ascii_case(&IcalComponentKind::VEvent)
        {
            let end = component
                .props
                .iter()
                .find(|p| p.name.eq_ignore_ascii_case("DTEND"))
                .and_then(instant);

            entries.push(TimeEntry {
                uid,
                task,
                summary,
                start,
                end,
                shape: Shape::Event,
                items: vec![item.clone()],
            });
            continue;
        }

        let mark = categories(component).iter().find_map(|category| {
            match category.to_lowercase().trim() {
                "start" => Some(true),
                "stop" => Some(false),
                _ => None,
            }
        });

        if let Some(start_mark) = mark {
            marks.push(Mark {
                uid,
                task,
                summary,
                at: start,
                start: start_mark,
                item: item.clone(),
            });
        }
    }

    marks.sort_by(|a, b| (&a.task, a.at, !a.start).cmp(&(&b.task, b.at, !b.start)));

    let mut open: Option<Mark> = None;

    for mark in marks {
        if let Some(started) = open.take_if(|started| started.task != mark.task) {
            entries.push(journal_entry(started, None));
        }

        match (open.take(), mark.start) {
            (Some(started), true) => {
                let at = mark.at;
                entries.push(journal_entry(started, Some((at, None))));
                open = Some(mark);
            }
            (Some(started), false) => {
                let at = mark.at;
                entries.push(journal_entry(started, Some((at, Some(mark.item)))));
            }
            (None, true) => open = Some(mark),
            (None, false) => (),
        }
    }

    if let Some(started) = open {
        entries.push(journal_entry(started, None));
    }

    entries
}

/// The entry a start mark opens, closed at `end` by the stop item given.
fn journal_entry(start: Mark, end: Option<(Timestamp, Option<RawItem>)>) -> TimeEntry {
    let mut items = vec![start.item];
    let end = end.map(|(at, stop)| {
        items.extend(stop);
        at
    });

    TimeEntry {
        uid: start.uid,
        task: start.task,
        summary: start.summary,
        start: start.at,
        end,
        shape: Shape::Journal,
        items,
    }
}

/// The first value of the property `name` of a component, as text.
fn value(component: &IcalComponent<'_>, name: &str) -> Option<String> {
    let prop = component
        .props
        .iter()
        .find(|prop| prop.name.eq_ignore_ascii_case(name))?;

    let value = match &prop.value {
        IcalValue::Text(text) => text.0.to_string(),
        IcalValue::TextList(list) => list.0.join(","),
        IcalValue::Uri(uri) => uri.0.to_string(),
        IcalValue::Unknown(_) => return None,
        _ => return None,
    };

    let value = value.trim().to_owned();
    (!value.is_empty()).then_some(value)
}

/// Every `CATEGORIES` value of a component.
fn categories(component: &IcalComponent<'_>) -> Vec<String> {
    component
        .props
        .iter()
        .filter(|prop| prop.name.eq_ignore_ascii_case("CATEGORIES"))
        .flat_map(|prop| match &prop.value {
            IcalValue::TextList(list) => list.0.iter().map(|value| value.to_string()).collect(),
            IcalValue::Text(text) => vec![text.0.to_string()],
            _ => Vec::new(),
        })
        .collect()
}

/// The instant a date-time property names: UTC with a trailing `Z`, in
/// its `TZID` zone, else in the local zone. A whole day names none.
pub fn instant(prop: &IcalProp<'_>) -> Option<Timestamp> {
    let IcalValue::DateTime(value) = &prop.value else {
        return None;
    };

    let value = value.0.trim();
    let (civil, utc) = match value.strip_suffix(['Z', 'z']) {
        Some(civil) => (civil, true),
        None => (value, false),
    };

    let civil = DateTime::strptime("%Y%m%dT%H%M%S", civil).ok()?;

    let zone = match utc {
        true => TimeZone::UTC,
        false => prop
            .params
            .iter()
            .find_map(|param| match param {
                IcalParam::TzId(tzid) => TimeZone::get(tzid.trim_start_matches('/')).ok(),
                _ => None,
            })
            .unwrap_or_else(TimeZone::system),
    };

    zone.to_ambiguous_zoned(civil)
        .compatible()
        .ok()
        .map(|zoned| zoned.timestamp())
}

/// An event entry for the task `task` of summary `summary`, started at
/// `start` and stopped at `end`, stamped at `now`.
pub fn event(
    uid: &str,
    task: &str,
    summary: &str,
    start: Timestamp,
    end: Option<Timestamp>,
    now: Timestamp,
) -> Vec<u8> {
    let mut props = vec![
        text(IcalPropKind::Uid, uid),
        prop(IcalPropKind::DtStamp, Vec::new(), utc_stamp(now)),
        prop(IcalPropKind::DtStart, Vec::new(), utc_stamp(start)),
    ];

    if let Some(end) = end {
        props.push(prop(IcalPropKind::DtEnd, Vec::new(), utc_stamp(end)));
    }

    props.extend([
        text(IcalPropKind::Summary, summary),
        text(IcalPropKind::Transp, "TRANSPARENT"),
        text(IcalPropKind::RelatedTo, task),
    ]);

    wrap(IcalComponentKind::VEvent, props)
}

/// One VJOURNAL of a pair: `start` or `stop` at `at`, for the task `task`
/// of summary `summary`.
pub fn journal(uid: &str, task: &str, summary: &str, at: Timestamp, start: bool) -> Vec<u8> {
    let mark = match start {
        true => "start",
        false => "stop",
    };

    let props = vec![
        text(IcalPropKind::Uid, uid),
        prop(IcalPropKind::DtStamp, Vec::new(), utc_stamp(at)),
        prop(IcalPropKind::DtStart, Vec::new(), utc_stamp(at)),
        text(IcalPropKind::Summary, summary),
        prop(
            IcalPropKind::Categories,
            Vec::new(),
            IcalValue::TextList(IcalTextList(vec![mark.into()])),
        ),
        text(IcalPropKind::RelatedTo, task),
    ];

    wrap(IcalComponentKind::VJournal, props)
}

/// Stops the running event of `contents` at `end`, stamped at `end`: its
/// `DTEND` written after its `DTSTART`, its `DTSTAMP` set where it stood,
/// every other line kept.
pub fn stop_event(contents: &[u8], end: Timestamp) -> Result<Vec<u8>> {
    let mut cst = IcalCst::parse(contents).context("Parse time entry error")?;
    let escaper = Escaper::for_version(cst.version());
    let line =
        |kind: IcalPropKind| IcalItem::Prop(prop(kind, Vec::new(), utc_stamp(end)).encode(escaper));

    let vevent = cst
        .component_mut::<VEVENT>()
        .context("Find the time entry's VEVENT error")?;

    vevent
        .items
        .retain(|item| !names(item, &IcalPropKind::DtEnd) && !names(item, &IcalPropKind::Duration));

    match vevent
        .items
        .iter()
        .position(|item| names(item, &IcalPropKind::DtStamp))
    {
        Some(at) => vevent.items[at] = line(IcalPropKind::DtStamp),
        None => vevent.items.insert(0, line(IcalPropKind::DtStamp)),
    }

    let at = vevent
        .items
        .iter()
        .position(|item| names(item, &IcalPropKind::DtStart))
        .map_or(vevent.items.len(), |at| at + 1);
    vevent.items.insert(at, line(IcalPropKind::DtEnd));

    Ok(cst.to_bytes())
}

/// The `UID` of the first component of `contents`.
pub fn uid_of(contents: &[u8]) -> Option<String> {
    let cst = IcalCst::parse(contents).ok()?;
    let ical = cst.decode();
    ical.components
        .iter()
        .find_map(|component| value(component, "UID"))
}

/// A calendar holding one component of `kind` made of `props`.
fn wrap(kind: IcalComponentKind, props: Vec<IcalProp<'static>>) -> Vec<u8> {
    let component = IcalComponent {
        name: IcalComponentName::Kind(kind),
        props,
        components: Vec::new(),
    };

    IcalCst::from(calendar(vec![component])).to_bytes()
}

#[cfg(test)]
mod tests {
    use jiff::Timestamp;

    use crate::{
        shared::ical::check,
        time::entry::{RawItem, Shape, event, journal, parse, stop_event},
    };

    fn at(stamp: &str) -> Timestamp {
        stamp.parse().unwrap()
    }

    fn raw(id: &str, contents: Vec<u8>) -> RawItem {
        RawItem {
            id: id.into(),
            etag: None,
            contents,
        }
    }

    #[test]
    fn a_running_event_has_no_end_and_a_stop_writes_it_after_the_start() {
        let running = event(
            "e1",
            "task-1",
            "Paint the hallway",
            at("2026-10-28T09:00:00Z"),
            None,
            at("2026-10-28T09:00:00Z"),
        );
        let text = String::from_utf8(running.clone()).unwrap();

        assert!(check(&running).is_empty(), "{:?}", check(&running));
        assert!(text.contains("TRANSP:TRANSPARENT\r\n"), "{text}");
        assert!(text.contains("RELATED-TO:task-1\r\n"), "{text}");
        assert!(!text.contains("DTEND"), "{text}");

        let entries = parse(vec![raw("e1.ics", running.clone())]);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].end, None);
        assert_eq!(entries[0].shape, Shape::Event);

        let stopped = stop_event(&running, at("2026-10-28T10:30:00Z")).unwrap();
        let text = String::from_utf8(stopped.clone()).unwrap();
        assert_eq!(
            text,
            String::from_utf8(running).unwrap().replace(
                "DTSTAMP:20261028T090000Z\r\nDTSTART:20261028T090000Z\r\n",
                "DTSTAMP:20261028T103000Z\r\nDTSTART:20261028T090000Z\r\n\
                     DTEND:20261028T103000Z\r\n"
            )
        );

        let entries = parse(vec![raw("e1.ics", stopped)]);
        assert_eq!(entries[0].end, Some(at("2026-10-28T10:30:00Z")));
    }

    #[test]
    fn journal_marks_pair_per_task_in_time_order() {
        let items = vec![
            raw(
                "4",
                journal("j4", "b", "B", at("2026-10-28T12:00:00Z"), true),
            ),
            raw(
                "2",
                journal("j2", "a", "A", at("2026-10-28T10:00:00Z"), false),
            ),
            raw(
                "1",
                journal("j1", "a", "A", at("2026-10-28T09:00:00Z"), true),
            ),
            raw(
                "0",
                journal("j0", "a", "A", at("2026-10-28T08:00:00Z"), false),
            ),
            raw(
                "3",
                journal("j3", "a", "A", at("2026-10-28T11:00:00Z"), true),
            ),
        ];

        assert!(check(&items[0].contents).is_empty());

        let mut entries: Vec<(String, Option<Timestamp>, usize)> = parse(items)
            .into_iter()
            .map(|entry| (entry.uid, entry.end, entry.items.len()))
            .collect();
        entries.sort();

        assert_eq!(
            entries,
            [
                ("j1".into(), Some(at("2026-10-28T10:00:00Z")), 2),
                ("j3".into(), None, 1),
                ("j4".into(), None, 1),
            ]
        );
    }

    #[test]
    fn an_event_relating_to_nothing_is_no_time_entry() {
        let meeting = b"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\n\
            BEGIN:VEVENT\r\nUID:m\r\nDTSTAMP:20261028T080000Z\r\n\
            DTSTART:20261028T090000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";

        assert!(parse(vec![raw("m.ics", meeting.to_vec())]).is_empty());
    }

    #[test]
    fn a_zoned_start_reads_in_its_zone() {
        let entry = b"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\n\
            BEGIN:VEVENT\r\nUID:z\r\nDTSTAMP:20261028T080000Z\r\n\
            DTSTART;TZID=Europe/Paris:20261028T100000\r\n\
            DTEND;TZID=Europe/Paris:20261028T110000\r\n\
            RELATED-TO:t\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";

        let entries = parse(vec![raw("z.ics", entry.to_vec())]);
        assert_eq!(entries[0].start, at("2026-10-28T09:00:00Z"));
        assert_eq!(entries[0].end, Some(at("2026-10-28T10:00:00Z")));
    }
}
