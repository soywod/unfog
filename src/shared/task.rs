//! # Task
//!
//! The VTODO projection over the shared items, and the `task` command
//! family built on it.
//!
//! A collection may mix component kinds, so the `task` commands read the
//! items of a tasklist and keep only the VTODOs. [`Task`] is that
//! projection, the properties a task list is read by. A projection never
//! writes: the edits go through [`fields`], which rewrites the lines it
//! names and keeps every other byte.

pub mod cli;
pub mod create;
pub mod delete;
pub mod done;
pub mod fields;
pub mod find;
pub mod gtd;
pub mod list;
pub mod read;
pub mod update;
#[cfg(test)]
mod vectors;

use ical::{
    component::{IcalComponent, IcalComponentKind},
    param::IcalParam,
    prop::IcalProp,
    tree::cst::IcalCst,
    value::IcalValue,
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::item::TaskItem;

/// A VTODO projected out of a [`TaskItem`]'s iCalendar bytes.
///
/// Times keep their iCalendar wire spelling, so what a listing prints is
/// what the tasklist carries.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Task {
    /// The handle of the item the task was projected from.
    pub id: String,
    /// The task's `UID`, empty when it carries none.
    pub uid: String,
    /// The task's `SUMMARY`, empty when it carries none.
    pub summary: String,
    /// The task's `DESCRIPTION`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// The task's `DTSTART`, verbatim.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start: Option<String>,
    /// The task's `DUE`, verbatim.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub due: Option<String>,
    /// The task's `STATUS` (`NEEDS-ACTION`, `IN-PROCESS`, `COMPLETED`,
    /// `CANCELLED`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status: Option<String>,
    /// The task's `COMPLETED`, verbatim.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub completed: Option<String>,
    /// The task's `PRIORITY`, 1 (highest) to 9 (lowest), 0 undefined.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<i64>,
    /// The task's `PERCENT-COMPLETE`, 0 to 100.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub percent_complete: Option<i64>,
    /// The task's `CATEGORIES`, every instance flattened in order.
    pub categories: Vec<String>,
    /// The `UID` its parent `RELATED-TO` names.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<String>,
    /// The task's `URL`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    /// The `TRIGGER` of each of the task's alarms, as written.
    pub alarms: Vec<String>,
    /// The task's `CREATED`, verbatim: when it was captured.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub created: Option<String>,
    /// The task's `DTSTAMP`, verbatim: when it was last written.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stamp: Option<String>,
    /// The task's `CONTACT`s: who it waits on.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub contacts: Vec<String>,
    /// The `Message-ID` of the message the task was captured from, its
    /// `ATTACH:mid:` (RFC 2392) bare of angle brackets.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mid: Option<String>,
}

impl Task {
    /// Projects every VTODO carried by `item`, in source order.
    ///
    /// An item whose bytes do not parse yields no task rather than an
    /// error, so one malformed resource does not stop the rest of the
    /// tasklist from rendering.
    pub fn project(item: &TaskItem) -> Vec<Self> {
        let Ok(cst) = IcalCst::parse(&item.contents) else {
            return Vec::new();
        };

        cst.decode()
            .components
            .iter()
            .filter(|component| {
                component
                    .name
                    .eq_ignore_ascii_case(&IcalComponentKind::VTodo)
            })
            .map(|vtodo| Self::from_vtodo(&item.id, vtodo))
            .collect()
    }

    /// The projection of one decoded VTODO.
    fn from_vtodo(id: &str, vtodo: &IcalComponent<'_>) -> Self {
        let mut task = Self {
            id: id.to_owned(),
            ..Self::default()
        };

        for prop in &vtodo.props {
            let value = value_str(prop);

            match prop.name.to_ascii_uppercase().as_str() {
                "UID" => task.uid = value.unwrap_or_default(),
                "SUMMARY" => task.summary = value.unwrap_or_default(),
                "DESCRIPTION" => task.description = value,
                "DTSTART" => task.start = value,
                "DUE" => task.due = value,
                "STATUS" => task.status = value.map(|status| status.to_ascii_uppercase()),
                "COMPLETED" => task.completed = value,
                "PRIORITY" => task.priority = value.and_then(|value| value.parse().ok()),
                "PERCENT-COMPLETE" => {
                    task.percent_complete = value.and_then(|value| value.parse().ok())
                }
                "CATEGORIES" => {
                    if let IcalValue::TextList(list) = &prop.value {
                        task.categories
                            .extend(list.0.iter().map(|value| value.to_string()));
                    } else if let Some(value) = value {
                        task.categories.push(value);
                    }
                }
                "RELATED-TO" if is_parent(prop) => task.parent = value,
                "URL" => task.url = value,
                "CREATED" => task.created = value,
                "DTSTAMP" => task.stamp = value,
                "CONTACT" => task.contacts.extend(value),
                "ATTACH" if task.mid.is_none() => task.mid = value.as_deref().and_then(mid),
                _ => (),
            }
        }

        task.alarms = vtodo
            .components
            .iter()
            .filter(|component| {
                component
                    .name
                    .eq_ignore_ascii_case(&IcalComponentKind::VAlarm)
            })
            .flat_map(|valarm| &valarm.props)
            .filter(|prop| prop.name.eq_ignore_ascii_case("TRIGGER"))
            .filter_map(trigger_str)
            .collect();

        task
    }

    /// Whether the task is still to be done: any status but `COMPLETED`
    /// and `CANCELLED`, no status included.
    pub fn is_open(&self) -> bool {
        !matches!(self.status.as_deref(), Some("COMPLETED" | "CANCELLED"))
    }

    /// The date-time the task is due at, as a sortable `YYYYMMDDTHHMMSS`
    /// key, a whole day reading as its midnight.
    ///
    /// The wall-clock spelling is compared, its zone set aside: close
    /// enough to order a list by, and keeping the CLI out of zone
    /// resolution.
    pub fn due_key(&self) -> Option<String> {
        let due = self.due.as_deref()?;
        let date = due.get(..8)?;
        let time = due.get(9..15).unwrap_or("000000");

        Some(format!("{date}T{time}"))
    }

    /// When the task was captured, as a sortable key, a task telling
    /// none last: its `CREATED`, else its `DTSTAMP`.
    pub fn captured_key(&self) -> (bool, String) {
        match self.created.as_ref().or(self.stamp.as_ref()) {
            Some(at) => (false, at.clone()),
            None => (true, String::new()),
        }
    }

    /// The completion percentage as a column value, empty when there is
    /// none.
    ///
    /// A COMPLETED status implies 100 even where the property is absent
    /// (RFC 5545 3.8.1.8), so a finished task never reads as unstarted.
    pub fn progress(&self) -> String {
        match self.percent_complete {
            Some(percent) => format!("{percent}%"),
            None if self.status.as_deref() == Some("COMPLETED") => String::from("100%"),
            None => String::new(),
        }
    }
}

/// The `Message-ID` a `mid:` URI (RFC 2392) names, percent-decoded and
/// bare of angle brackets; `None` for any other URI.
pub fn mid(uri: &str) -> Option<String> {
    let (scheme, id) = uri.trim().split_once(':')?;

    if !scheme.eq_ignore_ascii_case("mid") {
        return None;
    }

    // NOTE: a mid URI names the message alone; a `/` would name one of
    // its parts.
    let id = id.split('/').next().unwrap_or_default();
    let mut bytes = Vec::with_capacity(id.len());
    let mut chars = id.bytes();

    while let Some(byte) = chars.next() {
        match byte {
            b'%' => {
                let hex: Vec<u8> = chars.by_ref().take(2).collect();
                let hex = core::str::from_utf8(&hex).ok()?;
                bytes.push(u8::from_str_radix(hex, 16).ok()?);
            }
            byte => bytes.push(byte),
        }
    }

    let id = String::from_utf8(bytes).ok()?;
    let id = id.trim().trim_start_matches('<').trim_end_matches('>');
    (!id.is_empty()).then(|| id.to_owned())
}

/// Whether a `RELATED-TO` names the parent: `RELTYPE=PARENT`, or no type,
/// which defaults to it (RFC 5545 3.2.15).
fn is_parent(prop: &IcalProp<'_>) -> bool {
    prop.params.iter().all(|param| match param {
        IcalParam::RelType(reltype) => reltype.eq_ignore_ascii_case("PARENT"),
        _ => true,
    })
}

/// A property's value as text, `None` for a kind a projection does not
/// read.
fn value_str(prop: &IcalProp<'_>) -> Option<String> {
    let value = match &prop.value {
        IcalValue::Text(text) => text.0.to_string(),
        IcalValue::TextList(list) => list.0.join(","),
        IcalValue::Date(date) => date.0.to_string(),
        IcalValue::DateTime(stamp) => stamp.0.to_string(),
        IcalValue::Integer(integer) => integer.0.to_string(),
        IcalValue::Uri(uri) => uri.0.to_string(),
        _ => return None,
    };

    Some(value)
}

/// A `TRIGGER` as written: `-PT15M`, `-PT15M (end)` when it counts from
/// the due date, or the instant of an absolute one.
fn trigger_str(prop: &IcalProp<'_>) -> Option<String> {
    let value = match &prop.value {
        IcalValue::Duration(duration) => duration.0.to_string(),
        IcalValue::DateTime(stamp) => return Some(stamp.0.to_string()),
        _ => return None,
    };

    let from_end = prop.params.iter().any(
        |param| matches!(param, IcalParam::Related(related) if related.eq_ignore_ascii_case("END")),
    );

    match from_end {
        true => Some(format!("{value} (end)")),
        false => Some(value),
    }
}

#[cfg(test)]
mod tests {
    use crate::shared::{item::TaskItem, task::Task};

    const CALENDAR: &str = concat!(
        "BEGIN:VCALENDAR\r\n",
        "VERSION:2.0\r\n",
        "PRODID:-//x//y//EN\r\n",
        "BEGIN:VTODO\r\n",
        "UID:1@example.org\r\n",
        "DTSTAMP:20260101T000000Z\r\n",
        "DUE;VALUE=DATE:20260814\r\n",
        "SUMMARY:Write the report\r\n",
        "STATUS:IN-PROCESS\r\n",
        "PRIORITY:2\r\n",
        "CATEGORIES:work,writing\r\n",
        "CATEGORIES:q3\r\n",
        "RELATED-TO;RELTYPE=SIBLING:2@example.org\r\n",
        "RELATED-TO:0@example.org\r\n",
        "BEGIN:VALARM\r\n",
        "ACTION:DISPLAY\r\n",
        "DESCRIPTION:Write the report\r\n",
        "TRIGGER;RELATED=END:-PT15M\r\n",
        "END:VALARM\r\n",
        "END:VTODO\r\n",
        "BEGIN:VEVENT\r\n",
        "UID:2@example.org\r\n",
        "DTSTAMP:20260101T000000Z\r\n",
        "DTSTART:20260814T090000Z\r\n",
        "SUMMARY:Not a task\r\n",
        "END:VEVENT\r\n",
        "END:VCALENDAR\r\n",
    );

    fn item(contents: &str) -> TaskItem {
        TaskItem {
            id: "item-1".into(),
            tasklist_id: "personal".into(),
            etag: None,
            contents: contents.as_bytes().to_vec(),
        }
    }

    #[test]
    fn projection_keeps_vtodos_and_drops_every_other_kind() {
        let tasks = Task::project(&item(CALENDAR));

        assert_eq!(tasks.len(), 1);
        assert_eq!(tasks[0].id, "item-1");
        assert_eq!(tasks[0].uid, "1@example.org");
        assert_eq!(tasks[0].summary, "Write the report");
        assert_eq!(tasks[0].due.as_deref(), Some("20260814"));
        assert_eq!(tasks[0].status.as_deref(), Some("IN-PROCESS"));
        assert_eq!(tasks[0].priority, Some(2));
        assert_eq!(tasks[0].categories, ["work", "writing", "q3"]);
        assert_eq!(tasks[0].parent.as_deref(), Some("0@example.org"));
        assert_eq!(tasks[0].alarms, ["-PT15M (end)"]);
    }

    #[test]
    fn an_unparseable_item_projects_nothing_instead_of_failing() {
        assert!(Task::project(&item("not a calendar at all")).is_empty());
    }

    #[test]
    fn a_whole_day_sorts_as_its_midnight() {
        let task = Task {
            due: Some("20260814".into()),
            ..Default::default()
        };
        assert_eq!(task.due_key().as_deref(), Some("20260814T000000"));

        let task = Task {
            due: Some("20260814T170000Z".into()),
            ..Default::default()
        };
        assert_eq!(task.due_key().as_deref(), Some("20260814T170000"));
    }

    #[test]
    fn a_completed_task_reads_as_finished_even_with_no_percentage() {
        let completed = Task {
            status: Some("COMPLETED".into()),
            ..Default::default()
        };
        assert_eq!(completed.progress(), "100%");
        assert!(!completed.is_open());

        let started = Task {
            percent_complete: Some(40),
            ..Default::default()
        };
        assert_eq!(started.progress(), "40%");
        assert!(started.is_open());
    }
}
