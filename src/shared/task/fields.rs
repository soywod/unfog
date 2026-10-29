//! # Task fields
//!
//! The VTODO properties a create or an update sets from the command line,
//! how they are written onto a task, and the edits `done` and `undone`
//! make.
//!
//! They are a convenience over the common fields rather than the whole of
//! iCalendar: an iCalendar source and the composer are the complete
//! surface, which is why a recurrence, a contact, an attachment, a
//! `RELATED-TO` other than the parent and every X- property are not a
//! flag. A flag rewrites the property it names and leaves every other line
//! of the task byte for byte, which is what lets a layer above unfog keep
//! its own properties on a task.

use std::borrow::Cow;

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use ical::{
    component::{IcalComponent, IcalComponentKind, IcalComponentName, vtodo::VTODO},
    ical::Ical,
    param::IcalParam,
    prop::{IcalProp, IcalPropKind, IcalPropName, summary::SUMMARY},
    tree::{
        codec::mode::Escaper,
        cst::{IcalCst, IcalItem},
    },
    tzdb,
    validator::IcalValidateError,
    value::{
        IcalValue,
        datetime::{IcalDate, IcalDateTime},
        duration::IcalDuration,
        integer::IcalInteger,
        text::IcalText,
        uri::IcalUri,
    },
    version::IcalVersion,
};
use jiff::{
    Timestamp,
    civil::{Date, DateTime},
    tz::TimeZone,
};

use crate::shared::task::gtd::own_categories;

/// The VTODO fields `task create` and `task update` set from flags.
///
/// Each flag names one property, and writing it replaces every instance
/// of that property the task already carried. An empty value removes the
/// property instead.
#[derive(Debug, Default, Parser)]
pub struct TaskFieldsArgs {
    /// Title of the task (`SUMMARY`); empty removes it.
    #[arg(long, value_name = "TEXT")]
    pub summary: Option<String>,
    /// Free-form description (`DESCRIPTION`); empty removes it.
    #[arg(long, value_name = "TEXT")]
    pub description: Option<String>,
    /// When the task is due (`DUE`); empty removes it.
    ///
    /// `YYYY-MM-DD` for a whole day, `YYYY-MM-DDTHH:MM[:SS]` for a local
    /// time, the same with a trailing `Z` for UTC, or a time prefixed by
    /// an IANA zone: `Europe/Paris:2026-10-30T17:00`. A zone arrives with
    /// its `VTIMEZONE`. Replaces any `DURATION`.
    #[arg(long, value_name = "TIME")]
    pub due: Option<String>,
    /// When work on the task starts (`DTSTART`), spelled as `--due` is;
    /// empty removes it.
    #[arg(long, value_name = "TIME")]
    pub start: Option<String>,
    /// Status of the task (`STATUS`).
    ///
    /// `task done` and `task undone` also stamp the completion; this
    /// sets the status alone.
    #[arg(long, value_name = "STATUS")]
    pub status: Option<TaskStatusArg>,
    /// Priority of the task (`PRIORITY`), 1 the highest, 9 the lowest, 0
    /// undefined.
    #[arg(long, value_name = "0-9", value_parser = clap::value_parser!(u8).range(0..=9))]
    pub priority: Option<u8>,
    /// The person's own categories (`CATEGORIES`), the flag repeating,
    /// replacing theirs and keeping the GTD state (`inbox`, `waiting`,
    /// `someday`), which `gtd clarify` sets; `--category ""` alone removes
    /// them.
    #[arg(long = "category", value_name = "TEXT")]
    pub categories: Vec<String>,
    /// The `UID` of the parent task (`RELATED-TO;RELTYPE=PARENT`); empty
    /// removes it.
    ///
    /// Only the parent link is replaced: a `RELATED-TO` of another
    /// relationship type stays as it is.
    #[arg(long, value_name = "UID")]
    pub parent: Option<String>,
    /// Web page of the task (`URL`); empty removes it.
    #[arg(long, value_name = "URL")]
    pub url: Option<String>,
    /// One display alarm (`VALARM`), replacing every alarm the task has.
    ///
    /// A lead before the due date (`15m`, `1h`, `2d`, `1w`, or an RFC
    /// 5545 duration such as `PT15M`), written as
    /// `TRIGGER;RELATED=END:-<LEAD>` and needing a `DUE`; or a time,
    /// spelled as `--due` is, written as an absolute trigger in UTC.
    #[arg(long, value_name = "WHEN", conflicts_with = "no_alarm")]
    pub alarm: Option<String>,
    /// Remove every alarm (`VALARM`) of the task.
    #[arg(long)]
    pub no_alarm: bool,
}

/// The status a task can be set to (RFC 5545 3.8.1.11).
#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum TaskStatusArg {
    /// Not started (`NEEDS-ACTION`).
    NeedsAction,
    /// Under way (`IN-PROCESS`).
    InProcess,
    /// Finished (`COMPLETED`).
    Completed,
    /// Given up (`CANCELLED`).
    Cancelled,
}

impl TaskStatusArg {
    /// The `STATUS` value.
    pub fn as_status(self) -> &'static str {
        match self {
            Self::NeedsAction => "NEEDS-ACTION",
            Self::InProcess => "IN-PROCESS",
            Self::Completed => "COMPLETED",
            Self::Cancelled => "CANCELLED",
        }
    }
}

impl TaskFieldsArgs {
    /// Whether no field flag was given, the task then passing through.
    pub fn is_empty(&self) -> bool {
        self.summary.is_none()
            && self.description.is_none()
            && self.due.is_none()
            && self.start.is_none()
            && self.status.is_none()
            && self.priority.is_none()
            && self.categories.is_empty()
            && self.parent.is_none()
            && self.url.is_none()
            && self.alarm.is_none()
            && !self.no_alarm
    }

    /// Writes the flags onto the VTODO of `ical`, returning its new
    /// bytes.
    ///
    /// Every instance of a property a flag names is dropped and the flag's
    /// own is written in place of the first, so a flag sets that property
    /// rather than adding to it; an empty value writes nothing in its
    /// place, which removes it. Every other line keeps the task's own
    /// bytes, the properties no flag covers included.
    pub fn apply(&self, ical: &[u8]) -> Result<Vec<u8>> {
        if self.is_empty() {
            return Ok(ical.to_vec());
        }

        let mut cst = one_vtodo(ical)?;
        let escaper = Escaper::for_version(cst.version());

        let due = moment(self.due.as_deref())?;
        let start = moment(self.start.as_deref())?;

        if let (Some(Some(due)), Some(Some(start))) = (&due, &start)
            && due.is_date() != start.is_date()
        {
            bail!("--due and --start must both be dates or both be times");
        }

        let alarm = match &self.alarm {
            Some(when) => Some(Trigger::parse(when)?),
            None => None,
        };

        let (dropped, written) = self.props(due.as_ref(), start.as_ref())?;

        let lines: Lines = written
            .into_iter()
            .map(|(name, props)| {
                let items = props
                    .iter()
                    .map(|prop| IcalItem::Prop(prop.encode(escaper)))
                    .collect();
                (name, items)
            })
            .collect();

        let vtodo = master_mut(&mut cst)?;
        replace(vtodo, &dropped, lines);

        if self.no_alarm || alarm.is_some() {
            vtodo.items.retain(|item| !is_component(item, "VALARM"));
        }

        if let Some(trigger) = alarm {
            if trigger.is_lead() && !vtodo.items.iter().any(|item| names(item, "DUE")) {
                bail!("A lead alarm counts back from the due date; give the task a --due");
            }

            let summary = vtodo.prop::<SUMMARY>().map(|text| text.0.into_owned());
            vtodo.items.push(trigger.valarm(summary));
        }

        let zones: Vec<(String, i64)> = [&due, &start]
            .into_iter()
            .flatten()
            .flatten()
            .filter_map(|moment| match moment {
                Moment::Zoned(zone, _) => Some((zone.tzid.clone(), moment.anchor())),
                _ => None,
            })
            .collect();

        define_zones(&mut cst, &zones);

        // NOTE: the person's categories are replaced, the GTD state kept
        // where it stands (pimdir GUIDE.md §17).
        match self.categories.is_empty() {
            true => Ok(cst.to_bytes()),
            false => own_categories(&cst.to_bytes(), &self.categories),
        }
    }

    /// The properties the flags drop, and those they write in their
    /// place, grouped by name.
    ///
    /// A name holding no property is one an empty flag removes: it is
    /// dropped and nothing is written in its place.
    fn props(
        &self,
        due: Option<&Option<Moment>>,
        start: Option<&Option<Moment>>,
    ) -> Result<(Vec<Dropped>, Props)> {
        let mut dropped = Vec::new();
        let mut written = Vec::new();

        let mut push = |drop: Dropped, kind: IcalPropKind, values: Vec<IcalProp<'static>>| {
            dropped.push(drop);
            written.push((IcalPropName::Kind(kind), values));
        };

        for (kind, value) in [
            (IcalPropKind::Summary, &self.summary),
            (IcalPropKind::Description, &self.description),
        ] {
            if let Some(value) = value {
                push(
                    Dropped::Kind(kind),
                    kind,
                    unless_empty(value, |value| text(kind, value)),
                );
            }
        }

        if let Some(url) = &self.url {
            let url = unless_empty(url, |url| {
                let value = IcalValue::Uri(IcalUri(Cow::Owned(url.to_owned())));
                prop(IcalPropKind::Url, vec![], value)
            });
            push(Dropped::Kind(IcalPropKind::Url), IcalPropKind::Url, url);
        }

        if let Some(due) = due {
            let props = due.iter().map(|due| due.prop(IcalPropKind::Due)).collect();
            push(Dropped::Kind(IcalPropKind::Due), IcalPropKind::Due, props);

            // NOTE: a due date and a duration are one fact spelled twice
            // (RFC 5545 3.6.2 allows either, never both), so setting one
            // clears the other.
            push(
                Dropped::Kind(IcalPropKind::Duration),
                IcalPropKind::Duration,
                Vec::new(),
            );
        }

        if let Some(start) = start {
            let props = start
                .iter()
                .map(|start| start.prop(IcalPropKind::DtStart))
                .collect();
            push(
                Dropped::Kind(IcalPropKind::DtStart),
                IcalPropKind::DtStart,
                props,
            );
        }

        if let Some(status) = self.status {
            let prop = text(IcalPropKind::Status, status.as_status());
            push(
                Dropped::Kind(IcalPropKind::Status),
                IcalPropKind::Status,
                vec![prop],
            );
        }

        if let Some(priority) = self.priority {
            let value = IcalValue::Integer(IcalInteger(Cow::Owned(priority.to_string())));
            let prop = prop(IcalPropKind::Priority, vec![], value);
            push(
                Dropped::Kind(IcalPropKind::Priority),
                IcalPropKind::Priority,
                vec![prop],
            );
        }

        if let Some(parent) = &self.parent {
            let parent = unless_empty(parent, |uid| {
                let value = IcalValue::Text(IcalText(Cow::Owned(uid.to_owned())));
                let params = vec![IcalParam::RelType(Cow::Borrowed("PARENT"))];
                prop(IcalPropKind::RelatedTo, params, value)
            });
            push(Dropped::Parent, IcalPropKind::RelatedTo, parent);
        }

        Ok((dropped, written))
    }
}

/// Marks the VTODO of `ical` as done at `now` (RFC 5545 3.8.1.11,
/// 3.8.2.1, 3.8.1.8), returning its new bytes.
///
/// `STATUS:COMPLETED`, `COMPLETED` at `now` in UTC and
/// `PERCENT-COMPLETE:100`, each where it stood or after the task's last
/// property; every other line keeps its bytes.
pub fn complete(ical: &[u8], now: Timestamp) -> Result<Vec<u8>> {
    let mut cst = one_vtodo(ical)?;
    let escaper = Escaper::for_version(cst.version());

    let written = vec![
        (
            IcalPropKind::Status,
            text(IcalPropKind::Status, "COMPLETED"),
        ),
        (
            IcalPropKind::Completed,
            prop(IcalPropKind::Completed, vec![], utc_stamp(now)),
        ),
        (
            IcalPropKind::PercentComplete,
            prop(
                IcalPropKind::PercentComplete,
                vec![],
                IcalValue::Integer(IcalInteger(Cow::Borrowed("100"))),
            ),
        ),
    ];

    let dropped = written
        .iter()
        .map(|(kind, _)| Dropped::Kind(*kind))
        .collect::<Vec<_>>();

    let lines = written
        .into_iter()
        .map(|(kind, prop)| {
            let line = IcalItem::Prop(prop.encode(escaper));
            (IcalPropName::Kind(kind), vec![line])
        })
        .collect();

    let vtodo = master_mut(&mut cst)?;
    replace(vtodo, &dropped, lines);

    Ok(cst.to_bytes())
}

/// Puts the VTODO of `ical` back to `NEEDS-ACTION`, dropping its
/// `COMPLETED` and `PERCENT-COMPLETE`, and returns its new bytes.
pub fn reopen(ical: &[u8]) -> Result<Vec<u8>> {
    let mut cst = one_vtodo(ical)?;
    let escaper = Escaper::for_version(cst.version());

    let status = text(IcalPropKind::Status, "NEEDS-ACTION");
    let lines = vec![(
        IcalPropName::Kind(IcalPropKind::Status),
        vec![IcalItem::Prop(status.encode(escaper))],
    )];

    let dropped = [
        IcalPropKind::Status,
        IcalPropKind::Completed,
        IcalPropKind::PercentComplete,
    ]
    .map(Dropped::Kind);

    let vtodo = master_mut(&mut cst)?;
    replace(vtodo, &dropped, lines);

    Ok(cst.to_bytes())
}

/// Marks the VTODO of `ical` as captured at `now`: `CREATED` at `now` in
/// UTC right after its `DTSTAMP` (pimdir GUIDE.md §17 reads a capture's
/// age from it), and, for a capture of a message, `ATTACH` the `mid:` URI
/// (RFC 2392) naming `mid` after the task's last property.
pub fn captured(ical: &[u8], now: Timestamp, mid: Option<&str>) -> Result<Vec<u8>> {
    let mut cst = one_vtodo(ical)?;
    let escaper = Escaper::for_version(cst.version());
    let created =
        IcalItem::Prop(prop(IcalPropKind::Created, vec![], utc_stamp(now)).encode(escaper));

    let attach = mid.map(|mid| {
        let uri = IcalValue::Uri(IcalUri(Cow::Owned(mid_uri(mid))));
        let line = IcalItem::Prop(prop(IcalPropKind::Attach, vec![], uri).encode(escaper));
        (IcalPropName::Kind(IcalPropKind::Attach), vec![line])
    });

    let vtodo = master_mut(&mut cst)?;
    vtodo
        .items
        .retain(|item| !names(item, &IcalPropKind::Created));

    let at = vtodo
        .items
        .iter()
        .position(|item| names(item, &IcalPropKind::DtStamp))
        .map_or(0, |at| at + 1);
    vtodo.items.insert(at, created);

    if let Some(attach) = attach {
        replace(vtodo, &[], vec![attach]);
    }

    Ok(cst.to_bytes())
}

/// The `mid:` URI of a `Message-ID` (RFC 2392): bare of angle brackets,
/// every character a URI does not take as is percent-encoded.
pub fn mid_uri(mid: &str) -> String {
    let mid = mid.trim().trim_start_matches('<').trim_end_matches('>');
    let mut uri = String::from("mid:");

    for byte in mid.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' => uri.push(byte as char),
            b'!' | b'$' | b'&' | b'\'' | b'(' | b')' | b'*' | b'+' | b',' | b';' | b'=' => {
                uri.push(byte as char)
            }
            b':' | b'@' | b'.' | b'_' | b'~' | b'-' => uri.push(byte as char),
            byte => uri.push_str(&format!("%{byte:02X}")),
        }
    }

    uri
}

/// Marks every VTODO of `ical` as revised at `now`, returning its new
/// bytes (RFC 5545 3.8.7.2, 3.8.7.3).
///
/// `DTSTAMP` is set to `now` in UTC, where it stood, or after the task's
/// last property when it carried none; `LAST-MODIFIED` is set to the same
/// instant only where the task carries one. Every other line keeps its
/// bytes, and a source holding no VTODO comes back as it was, as does one
/// the parser cannot structure.
pub fn revise(ical: &[u8], now: Timestamp) -> Result<Vec<u8>> {
    let mut recovery = IcalCst::parse_recovering(ical);

    if !recovery.is_clean() {
        return Ok(ical.to_vec());
    }

    let stamp = utc_stamp(now);

    for cst in &mut recovery.calendars {
        let escaper = Escaper::for_version(cst.version());

        for item in &mut cst.items {
            if !is_component(item, "VTODO") {
                continue;
            }

            let IcalItem::Component(vtodo) = item else {
                continue;
            };

            let mut kinds = vec![IcalPropKind::DtStamp];

            if vtodo
                .items
                .iter()
                .any(|item| names(item, &IcalPropKind::LastModified))
            {
                kinds.push(IcalPropKind::LastModified);
            }

            let mut dropped = Vec::new();
            let mut lines: Lines = Vec::new();

            for kind in kinds {
                let line = IcalItem::Prop(prop(kind, Vec::new(), stamp.clone()).encode(escaper));
                dropped.push(Dropped::Kind(kind));
                lines.push((IcalPropName::Kind(kind), vec![line]));
            }

            replace(vtodo, &dropped, lines);
        }
    }

    Ok(recovery.to_bytes())
}

/// Parses `ical`, refusing a source holding anything but one VTODO.
///
/// An edit rewrites one task, and the parser reads the first calendar
/// alone, so a source holding several would come back with all but one
/// dropped or left inconsistent with it.
pub(crate) fn one_vtodo(ical: &[u8]) -> Result<IcalCst<'_>> {
    let calendars: Vec<IcalCst<'_>> = IcalCst::parse_many(ical)
        .filter_map(|cst| cst.ok())
        .collect();
    let vtodos: usize = calendars
        .iter()
        .map(|cst| cst.components::<VTODO>().count())
        .sum();

    let cst = IcalCst::parse(ical).context("Parse iCalendar error")?;

    if vtodos == 0 || cst.component::<VTODO>().is_none() {
        bail!("Cannot edit a source holding no VTODO");
    }

    if vtodos > 1 && (calendars.len() > 1 || !is_series(&cst)) {
        bail!("Cannot edit a source holding several VTODOs");
    }

    Ok(cst)
}

/// Whether the VTODOs of `cst` are one recurring task: one `UID`, one
/// master stating no `RECURRENCE-ID`, the rest its overrides.
fn is_series(cst: &IcalCst<'_>) -> bool {
    let decoded = cst.decode();
    let vtodos: Vec<&IcalComponent<'_>> = decoded
        .components
        .iter()
        .filter(|component| {
            component
                .name
                .eq_ignore_ascii_case(&IcalComponentKind::VTodo)
        })
        .collect();

    fn uid(vtodo: &IcalComponent<'_>) -> Option<String> {
        vtodo
            .props
            .iter()
            .find(|prop| prop.name.eq_ignore_ascii_case("UID"))
            .and_then(|prop| match &prop.value {
                IcalValue::Text(text) => Some(text.0.trim().to_owned()),
                _ => None,
            })
    }

    let masters = vtodos
        .iter()
        .filter(|vtodo| {
            !vtodo
                .props
                .iter()
                .any(|prop| prop.name.eq_ignore_ascii_case("RECURRENCE-ID"))
        })
        .count();

    masters == 1 && vtodos.iter().all(|vtodo| uid(vtodo) == uid(vtodos[0]))
}

/// The VTODO an edit writes: the only one, or the master of a recurring
/// task, its overrides kept as they are.
pub(crate) fn master_mut<'c, 'a>(cst: &'c mut IcalCst<'a>) -> Result<&'c mut IcalCst<'a>> {
    let at = cst.items.iter().position(|item| match item {
        IcalItem::Component(vtodo) if is_component(item, "VTODO") => {
            !vtodo.items.iter().any(|line| names(line, "RECURRENCE-ID"))
        }
        _ => false,
    });

    match at.map(|at| &mut cst.items[at]) {
        Some(IcalItem::Component(vtodo)) => Ok(vtodo),
        _ => bail!("Find VTODO error"),
    }
}

/// What a flag drops from the task before writing its own lines.
#[derive(Clone, Copy, Debug)]
enum Dropped {
    /// Every instance of one property.
    Kind(IcalPropKind),
    /// The `RELATED-TO` lines naming the parent: those of
    /// `RELTYPE=PARENT`, and those naming no type, which defaults to it
    /// (RFC 5545 3.2.15).
    Parent,
}

impl Dropped {
    /// Whether `item` is a line this drops.
    fn drops(&self, item: &IcalItem<'_>) -> bool {
        match self {
            Self::Kind(kind) => names(item, kind),
            Self::Parent => {
                let IcalItem::Prop(line) = item else {
                    return false;
                };

                if !line.name.get().eq_ignore_ascii_case("RELATED-TO") {
                    return false;
                }

                let reltype = line
                    .params
                    .iter()
                    .find(|param| param.name.get().eq_ignore_ascii_case("RELTYPE"));

                match reltype {
                    None => true,
                    Some(param) => param
                        .values
                        .iter()
                        .any(|value| value.get().trim_matches('"').eq_ignore_ascii_case("PARENT")),
                }
            }
        }
    }

    /// The property name the lines replacing these are written under.
    fn name(&self) -> &str {
        match self {
            Self::Kind(kind) => kind,
            Self::Parent => "RELATED-TO",
        }
    }
}

/// Encoded lines, grouped by the property they spell.
type Lines = Vec<(IcalPropName<'static>, Vec<IcalItem<'static>>)>;

/// Properties to write, grouped by the property they spell.
type Props = Vec<(IcalPropName<'static>, Vec<IcalProp<'static>>)>;

/// The one property a value stands for, none when it is empty: what an
/// empty flag writes is its property's removal.
fn unless_empty(
    value: &str,
    prop: impl FnOnce(&str) -> IcalProp<'static>,
) -> Vec<IcalProp<'static>> {
    match value.is_empty() {
        true => Vec::new(),
        false => vec![prop(value)],
    }
}

/// Reads a time flag, `None` standing for no flag and `Some(None)` for an
/// empty one, the property's removal.
fn moment(value: Option<&str>) -> Result<Option<Option<Moment>>> {
    match value {
        None => Ok(None),
        Some(value) if value.trim().is_empty() => Ok(Some(None)),
        Some(value) => Ok(Some(Some(Moment::parse(value)?))),
    }
}

/// An instant as an iCalendar UTC date-time value.
pub(crate) fn utc_stamp(at: Timestamp) -> IcalValue<'static> {
    IcalValue::DateTime(IcalDateTime(Cow::Owned(
        at.strftime("%Y%m%dT%H%M%SZ").to_string(),
    )))
}

/// When an alarm fires.
#[derive(Clone, Debug)]
enum Trigger {
    /// A lead before the due date, as an RFC 5545 duration with no sign.
    Lead(String),
    /// An instant, as `YYYYMMDDTHHMMSSZ`.
    At(String),
}

impl Trigger {
    /// Parses an `--alarm` value: a short lead, an RFC 5545 duration, or
    /// a time.
    fn parse(value: &str) -> Result<Self> {
        let value = value.trim();

        if let Some(lead) = lead(value)? {
            return Ok(Self::Lead(lead));
        }

        let instant = match Moment::parse(value)? {
            Moment::Date(_) => bail!(
                "An alarm fires at a time, not on a day; give `{value}T09:00` or a lead such \
                 as 1d"
            ),
            Moment::Floating(civil) => TimeZone::system()
                .to_ambiguous_zoned(civil)
                .compatible()?
                .timestamp(),
            Moment::Utc(civil) => civil.to_zoned(TimeZone::UTC)?.timestamp(),
            Moment::Zoned(zone, civil) => {
                zone.tz.to_ambiguous_zoned(civil).compatible()?.timestamp()
            }
        };

        Ok(Self::At(instant.strftime("%Y%m%dT%H%M%SZ").to_string()))
    }

    /// Whether the trigger counts back from the due date.
    fn is_lead(&self) -> bool {
        matches!(self, Self::Lead(_))
    }

    /// The display alarm this trigger fires, `summary` its text.
    ///
    /// RFC 5545 3.6.6 owes a display alarm a `DESCRIPTION`: the task's
    /// summary, or a plain word when it has none.
    fn valarm(&self, summary: Option<String>) -> IcalItem<'static> {
        let trigger = match self {
            Self::Lead(lead) => prop(
                IcalPropKind::Trigger,
                vec![IcalParam::Related(Cow::Borrowed("END"))],
                IcalValue::Duration(IcalDuration(Cow::Owned(format!("-{lead}")))),
            ),
            Self::At(at) => prop(
                IcalPropKind::Trigger,
                vec![IcalParam::Value(Cow::Borrowed("DATE-TIME"))],
                IcalValue::DateTime(IcalDateTime(Cow::Owned(at.clone()))),
            ),
        };

        let description = summary
            .filter(|summary| !summary.is_empty())
            .unwrap_or_else(|| String::from("Reminder"));

        let valarm = IcalComponent {
            name: IcalComponentName::Kind(IcalComponentKind::VAlarm),
            props: vec![
                text(IcalPropKind::Action, "DISPLAY"),
                text(IcalPropKind::Description, &description),
                trigger,
            ],
            components: Vec::new(),
        };

        let calendar = Ical {
            version: IcalVersion::V2_0,
            props: Vec::new(),
            components: vec![valarm],
        };

        IcalCst::from(calendar)
            .items
            .into_iter()
            .find(|item| matches!(item, IcalItem::Component(_)))
            .expect("a calendar built around one component holds it")
    }
}

/// Reads a lead: `<N>m`, `<N>h`, `<N>d`, `<N>w`, or an RFC 5545 duration,
/// a leading minus tolerated since a lead counts back anyway.
///
/// `None` when the value is no lead at all, which leaves it to be read as
/// a time. A duration outside the grammar, or of zero length, is refused.
fn lead(value: &str) -> Result<Option<String>> {
    let short = value
        .char_indices()
        .last()
        .filter(|(at, _)| *at > 0 && value[..*at].bytes().all(|byte| byte.is_ascii_digit()));

    let duration = match short {
        Some((at, unit)) => {
            let count: u64 = value[..at].parse()?;

            match unit.to_ascii_lowercase() {
                's' => format!("PT{count}S"),
                'm' => format!("PT{count}M"),
                'h' => format!("PT{count}H"),
                'd' => format!("P{count}D"),
                'w' => format!("P{count}W"),
                _ => return Ok(None),
            }
        }
        None => {
            let upper = value.to_ascii_uppercase();
            let unsigned = upper.trim_start_matches(['-', '+']);

            if !unsigned.starts_with('P') {
                return Ok(None);
            }

            unsigned.to_owned()
        }
    };

    let length = IcalValue::Duration(IcalDuration(Cow::Owned(duration.clone())));
    let valarm = IcalComponent {
        name: IcalComponentName::Kind(IcalComponentKind::VTodo),
        props: vec![prop(IcalPropKind::Duration, Vec::new(), length)],
        components: Vec::new(),
    };

    let ical = Ical {
        version: IcalVersion::V2_0,
        props: Vec::new(),
        components: vec![valarm],
    };

    let conforms = match ical.validate() {
        Ok(_) => true,
        Err(errors) => !errors
            .iter()
            .any(|err| matches!(err, IcalValidateError::Duration { .. })),
    };

    let zero = !duration.bytes().any(|byte| matches!(byte, b'1'..=b'9'));

    if !conforms || zero {
        bail!(
            "Cannot read alarm lead `{value}`; give 15m, 1h, 2d, 1w or an RFC 5545 duration \
             such as PT15M"
        );
    }

    Ok(Some(duration))
}

/// An IANA zone a time names.
#[derive(Clone, Debug)]
struct Zone {
    /// The name, as the database spells it.
    tzid: String,
    /// The zone itself.
    tz: TimeZone,
}

impl Zone {
    /// Resolves an IANA name, refusing one the database does not know.
    fn named(name: &str) -> Result<Self> {
        let Ok(tz) = TimeZone::get(name) else {
            bail!("Unknown time zone `{name}`; give an IANA name such as `Europe/Paris`");
        };

        let tzid = tz.iana_name().unwrap_or(name).to_owned();

        Ok(Self { tzid, tz })
    }
}

/// A time a flag names.
#[derive(Clone, Debug)]
enum Moment {
    /// A whole day.
    Date(Date),
    /// A local time, read wherever it is read.
    Floating(DateTime),
    /// A time in UTC.
    Utc(DateTime),
    /// A time in a named zone.
    Zoned(Zone, DateTime),
}

impl Moment {
    /// Parses a time flag.
    fn parse(value: &str) -> Result<Self> {
        let value = value.trim();

        // NOTE: a time starts with its year, and a zone name never starts
        // with a digit, so a leading letter announces a zone prefix.
        let (prefix, time) = match value.starts_with(|c: char| c.is_ascii_alphabetic()) {
            true => match value.split_once(':') {
                Some((prefix, time)) => (Some(Zone::named(prefix)?), time),
                None => bail!("Cannot read time `{value}`; {}", Self::SHAPES),
            },
            false => (None, value),
        };

        if let Ok(date) = Date::strptime("%Y-%m-%d", time) {
            if prefix.is_some() {
                bail!("Cannot read time `{value}`: a whole day names no zone");
            }

            return Ok(Self::Date(date));
        }

        let (time, utc) = match time.strip_suffix(['Z', 'z']) {
            Some(time) => (time, true),
            None => (time, false),
        };

        let Some(civil) = ["%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M"]
            .iter()
            .find_map(|format| DateTime::strptime(format, time).ok())
        else {
            bail!("Cannot read time `{value}`; {}", Self::SHAPES);
        };

        match (prefix, utc) {
            (Some(_), true) => bail!("Cannot read time `{value}`: it names a zone and UTC both"),
            (Some(zone), false) => Ok(Self::Zoned(zone, civil)),
            (None, true) => Ok(Self::Utc(civil)),
            (None, false) => Ok(Self::Floating(civil)),
        }
    }

    /// The shapes a time flag takes, for an error naming them.
    const SHAPES: &str = "give YYYY-MM-DD, YYYY-MM-DDTHH:MM[:SS], the same with a trailing Z, \
                          or prefixed by an IANA zone (Europe/Paris:2026-10-30T17:00)";

    /// Whether the time is a whole day.
    fn is_date(&self) -> bool {
        matches!(self, Self::Date(_))
    }

    /// The instant the time stands for, a whole day and a local time read
    /// as UTC.
    fn anchor(&self) -> i64 {
        let instant = match self {
            Self::Date(date) => date.to_zoned(TimeZone::UTC).map(|at| at.timestamp()),
            Self::Floating(civil) | Self::Utc(civil) => {
                civil.to_zoned(TimeZone::UTC).map(|at| at.timestamp())
            }
            Self::Zoned(zone, civil) => zone
                .tz
                .to_ambiguous_zoned(*civil)
                .compatible()
                .map(|at| at.timestamp()),
        };

        instant.map(Timestamp::as_second).unwrap_or_default()
    }

    /// The property spelling the time: `VALUE=DATE` for a whole day, a
    /// trailing `Z` for UTC, a `TZID` for a zone.
    fn prop(&self, kind: IcalPropKind) -> IcalProp<'static> {
        let stamp = |civil: &DateTime| civil.strftime("%Y%m%dT%H%M%S").to_string();

        let (params, value) = match self {
            Self::Date(date) => (
                vec![IcalParam::Value(Cow::Borrowed("DATE"))],
                IcalValue::Date(IcalDate(Cow::Owned(date.strftime("%Y%m%d").to_string()))),
            ),
            Self::Floating(civil) => (
                Vec::new(),
                IcalValue::DateTime(IcalDateTime(Cow::Owned(stamp(civil)))),
            ),
            Self::Utc(civil) => (
                Vec::new(),
                IcalValue::DateTime(IcalDateTime(Cow::Owned(format!("{}Z", stamp(civil))))),
            ),
            Self::Zoned(zone, civil) => (
                vec![IcalParam::TzId(Cow::Owned(zone.tzid.clone()))],
                IcalValue::DateTime(IcalDateTime(Cow::Owned(stamp(civil)))),
            ),
        };

        prop(kind, params, value)
    }
}

/// Drops every line `dropped` names from a component and writes `lines`
/// in their place.
///
/// The lines of a name land where its first dropped line stood, so a flag
/// leaves the layout as it found it; a name the component did not carry
/// lands after its last property, ahead of any nested component, as RFC
/// 5545 orders a component's body.
fn replace(component: &mut IcalCst<'_>, dropped: &[Dropped], mut lines: Lines) {
    let items = std::mem::take(&mut component.items);
    let mut kept = Vec::with_capacity(items.len());

    for item in items {
        let Some(drop) = dropped.iter().find(|drop| drop.drops(&item)) else {
            kept.push(item);
            continue;
        };

        if let Some(index) = lines
            .iter()
            .position(|(written, _)| written.eq_ignore_ascii_case(drop.name()))
        {
            kept.extend(lines.remove(index).1);
        }
    }

    let at = kept
        .iter()
        .position(|item| matches!(item, IcalItem::Component(_)))
        .unwrap_or(kept.len());

    kept.splice(at..at, lines.into_iter().flat_map(|(_, items)| items));
    component.items = kept;
}

/// Gives every zone a flag named its `VTIMEZONE`, ahead of the VTODO.
///
/// One the calendar already defines is left as it is, and one minted is
/// minted once, described around the first time naming it.
fn define_zones(cst: &mut IcalCst<'_>, zones: &[(String, i64)]) {
    for (tzid, anchor) in zones {
        let defined = cst
            .components::<ical::component::vtimezone::VTIMEZONE>()
            .any(|vtimezone| {
                vtimezone
                    .items
                    .iter()
                    .filter_map(|item| match item {
                        IcalItem::Prop(line) if line.name.get().eq_ignore_ascii_case("TZID") => {
                            Some(line.raw_value_str())
                        }
                        _ => None,
                    })
                    .any(|defined| defined.eq_ignore_ascii_case(tzid))
            });

        if defined {
            continue;
        }

        // NOTE: a zone the database cannot describe stands on its TZID
        // alone, which every reader resolves through the database anyway.
        let Some(vtimezone) = tzdb::vtimezone(tzid, *anchor) else {
            continue;
        };

        let at = cst
            .items
            .iter()
            .position(|item| is_component(item, "VTODO"))
            .unwrap_or(cst.items.len());

        cst.items
            .insert(at, IcalItem::Component(Box::new(vtimezone)));
    }
}

/// Whether an item is a nested component of the given name.
pub(crate) fn is_component(item: &IcalItem<'_>, name: &str) -> bool {
    match item {
        IcalItem::Component(child) => child
            .begin
            .as_ref()
            .is_some_and(|begin| begin.raw_value_str().eq_ignore_ascii_case(name)),
        _ => false,
    }
}

/// Whether an item is a line carrying the given property.
pub(crate) fn names(item: &IcalItem<'_>, name: &str) -> bool {
    match item {
        IcalItem::Prop(line) => line.name.get().eq_ignore_ascii_case(name),
        _ => false,
    }
}

/// A text property, its line breaks normalized to the `\n` RFC 5545
/// escapes.
pub(crate) fn text(kind: IcalPropKind, value: &str) -> IcalProp<'static> {
    let value = value.replace("\r\n", "\n").replace('\r', "\n");
    prop(
        kind,
        Vec::new(),
        IcalValue::Text(IcalText(Cow::Owned(value))),
    )
}

/// A property under a canonical name.
pub(crate) fn prop(
    kind: IcalPropKind,
    params: Vec<IcalParam<'static>>,
    value: IcalValue<'static>,
) -> IcalProp<'static> {
    IcalProp {
        name: IcalPropName::Kind(kind),
        params,
        value,
    }
}

#[cfg(test)]
mod tests {
    use clap::Parser;
    use jiff::Timestamp;

    use crate::shared::task::fields::{TaskFieldsArgs, complete, reopen, revise};

    #[derive(Parser)]
    struct Wrap {
        #[command(flatten)]
        fields: TaskFieldsArgs,
    }

    fn fields(args: &[&str]) -> TaskFieldsArgs {
        Wrap::try_parse_from([&["unfog"], args].concat())
            .unwrap()
            .fields
    }

    fn apply(ical: &str, args: &[&str]) -> String {
        let out = fields(args).apply(ical.as_bytes()).unwrap();
        String::from_utf8(out).unwrap()
    }

    fn refusal(ical: &str, args: &[&str]) -> String {
        fields(args).apply(ical.as_bytes()).unwrap_err().to_string()
    }

    /// A task another client wrote: a folded line, an X- property with a
    /// parameter, two kinds of `RELATED-TO` and an alarm.
    const TASK: &str = concat!(
        "BEGIN:VCALENDAR\r\n",
        "VERSION:2.0\r\n",
        "PRODID:-//x//y//EN\r\n",
        "BEGIN:VTODO\r\n",
        "UID:1@example.org\r\n",
        "DTSTAMP:20260101T000000Z\r\n",
        "SUMMARY:Write the report\r\n",
        "DESCRIPTION:A description long enough to be folded by the client that \r\n",
        " wrote it\r\n",
        "DURATION:PT1H\r\n",
        "CATEGORIES:work\r\n",
        "CATEGORIES:q3\r\n",
        "RELATED-TO;RELTYPE=SIBLING:2@example.org\r\n",
        "RELATED-TO:0@example.org\r\n",
        "X-PIMALAYA-GTD;CONTEXT=office:next\r\n",
        "BEGIN:VALARM\r\n",
        "ACTION:DISPLAY\r\n",
        "DESCRIPTION:Old\r\n",
        "TRIGGER:-PT10M\r\n",
        "END:VALARM\r\n",
        "END:VTODO\r\n",
        "END:VCALENDAR\r\n",
    );

    #[test]
    fn no_flag_leaves_the_task_byte_for_byte() {
        assert_eq!(apply(TASK, &[]), TASK);
        assert_eq!(apply("not an ical", &[]), "not an ical");
    }

    #[test]
    fn a_flag_rewrites_its_line_and_nothing_else() {
        let out = apply(TASK, &["--summary", "Write the annual report"]);

        assert_eq!(
            out,
            TASK.replace(
                "SUMMARY:Write the report\r\n",
                "SUMMARY:Write the annual report\r\n"
            )
        );
    }

    #[test]
    fn a_repeated_flag_replaces_every_instance_where_the_first_stood() {
        let out = apply(TASK, &["--category", "home", "--category", "q4"]);

        assert_eq!(
            out,
            TASK.replace(
                "CATEGORIES:work\r\nCATEGORIES:q3\r\n",
                "CATEGORIES:home,q4\r\n"
            )
        );

        let out = apply(TASK, &["--category", ""]);
        assert!(!out.contains("CATEGORIES"), "{out}");
    }

    #[test]
    fn an_empty_flag_removes_its_property() {
        let out = apply(TASK, &["--description", ""]);

        assert_eq!(
            out,
            TASK.replace(
                "DESCRIPTION:A description long enough to be folded by the client that \r\n \
                 wrote it\r\n",
                ""
            )
        );
    }

    #[test]
    fn a_due_date_replaces_the_duration_and_an_empty_one_removes_it() {
        let out = apply(TASK, &["--due", "2026-10-30T17:00:00Z"]);

        assert!(out.contains("DUE:20261030T170000Z\r\n"), "{out}");
        assert!(!out.contains("DURATION"), "{out}");

        let out = apply(&out, &["--due", ""]);
        assert!(!out.contains("DUE"), "{out}");
    }

    #[test]
    fn a_time_takes_a_date_a_local_time_a_utc_one_and_a_zone() {
        let out = apply(TASK, &["--due", "2026-10-30"]);
        assert!(out.contains("DUE;VALUE=DATE:20261030\r\n"), "{out}");

        let out = apply(TASK, &["--start", "2026-10-30T09:00"]);
        assert!(out.contains("DTSTART:20261030T090000\r\n"), "{out}");

        let out = apply(TASK, &["--due", "Europe/Paris:2026-10-30T17:00"]);
        assert!(
            out.contains("DUE;TZID=Europe/Paris:20261030T170000\r\n"),
            "{out}"
        );
        assert!(
            out.contains("BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\n"),
            "{out}"
        );
        assert!(out.find("BEGIN:VTIMEZONE").unwrap() < out.find("BEGIN:VTODO").unwrap());

        let err = refusal(TASK, &["--due", "tomorrow"]);
        assert!(err.contains("YYYY-MM-DD"), "{err}");

        let err = refusal(
            TASK,
            &["--due", "2026-10-30", "--start", "2026-10-29T09:00"],
        );
        assert!(err.contains("both be dates"), "{err}");
    }

    #[test]
    fn the_parent_flag_replaces_the_parent_link_alone() {
        let out = apply(TASK, &["--parent", "9@example.org"]);

        assert!(
            out.contains("RELATED-TO;RELTYPE=SIBLING:2@example.org\r\n"),
            "{out}"
        );
        assert!(
            out.contains("RELATED-TO;RELTYPE=PARENT:9@example.org\r\n"),
            "{out}"
        );
        assert!(!out.contains("0@example.org"), "{out}");
    }

    #[test]
    fn status_priority_and_url_write_their_property() {
        let out = apply(
            TASK,
            &[
                "--status",
                "in-process",
                "--priority",
                "1",
                "--url",
                "https://example.org/t/1",
            ],
        );

        assert!(out.contains("STATUS:IN-PROCESS\r\n"), "{out}");
        assert!(out.contains("PRIORITY:1\r\n"), "{out}");
        assert!(out.contains("URL:https://example.org/t/1\r\n"), "{out}");

        // NOTE: a property the task lacked lands after its last one,
        // ahead of the alarm, as RFC 5545 orders a body.
        assert!(out.find("URL:").unwrap() < out.find("BEGIN:VALARM").unwrap());

        assert!(Wrap::try_parse_from(["unfog", "--priority", "10"]).is_err());
    }

    #[test]
    fn a_lead_alarm_replaces_every_alarm_and_counts_back_from_the_due_date() {
        let due = apply(TASK, &["--due", "2026-10-30T17:00:00Z"]);
        let out = apply(&due, &["--alarm", "15m"]);

        assert_eq!(out.matches("BEGIN:VALARM").count(), 1, "{out}");
        assert!(!out.contains("TRIGGER:-PT10M"), "{out}");
        assert!(
            out.contains(concat!(
                "BEGIN:VALARM\r\n",
                "ACTION:DISPLAY\r\n",
                "DESCRIPTION:Write the report\r\n",
                "TRIGGER;RELATED=END:-PT15M\r\n",
                "END:VALARM\r\n",
                "END:VTODO\r\n",
            )),
            "{out}"
        );

        for (lead, trigger) in [
            ("1h", "-PT1H"),
            ("2d", "-P2D"),
            ("1w", "-P1W"),
            ("PT90M", "-PT90M"),
            ("-p1dt2h", "-P1DT2H"),
        ] {
            let flag = format!("--alarm={lead}");
            let out = apply(&due, &[&flag]);
            assert!(
                out.contains(&format!("TRIGGER;RELATED=END:{trigger}\r\n")),
                "{lead}: {out}"
            );
        }
    }

    #[test]
    fn a_lead_alarm_needs_a_due_date_and_a_lead_the_grammar_takes() {
        let err = refusal(TASK, &["--alarm", "15m"]);
        assert!(err.contains("--due"), "{err}");

        // NOTE: the due date may come with the alarm.
        let out = apply(TASK, &["--due", "2026-10-30", "--alarm", "1d"]);
        assert!(out.contains("TRIGGER;RELATED=END:-P1D\r\n"), "{out}");

        let err = refusal(TASK, &["--due", "2026-10-30", "--alarm", "0m"]);
        assert!(err.contains("Cannot read alarm lead"), "{err}");

        let err = refusal(TASK, &["--due", "2026-10-30", "--alarm", "PT1X"]);
        assert!(err.contains("Cannot read alarm lead"), "{err}");
    }

    #[test]
    fn an_absolute_alarm_fires_at_a_utc_instant() {
        let out = apply(TASK, &["--alarm", "2026-10-30T08:00:00Z"]);
        assert!(
            out.contains("TRIGGER;VALUE=DATE-TIME:20261030T080000Z\r\n"),
            "{out}"
        );

        let out = apply(TASK, &["--alarm", "Europe/Paris:2026-10-30T09:00"]);
        assert!(
            out.contains("TRIGGER;VALUE=DATE-TIME:20261030T080000Z\r\n"),
            "{out}"
        );

        let err = refusal(TASK, &["--alarm", "2026-10-30"]);
        assert!(err.contains("not on a day"), "{err}");
    }

    #[test]
    fn no_alarm_removes_every_alarm_and_refuses_an_alarm_beside_it() {
        let out = apply(TASK, &["--no-alarm"]);
        assert!(!out.contains("VALARM"), "{out}");
        assert!(
            out.contains("X-PIMALAYA-GTD;CONTEXT=office:next\r\n"),
            "{out}"
        );

        assert!(Wrap::try_parse_from(["unfog", "--alarm", "1h", "--no-alarm"]).is_err());
    }

    #[test]
    fn a_source_holding_several_vtodos_or_none_is_refused() {
        let two = TASK.replace(
            "END:VCALENDAR\r\n",
            "BEGIN:VTODO\r\nUID:2\r\nDTSTAMP:20260101T000000Z\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
        );
        let err = refusal(&two, &["--summary", "x"]);
        assert!(err.contains("several VTODOs"), "{err}");

        let none = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\n\
                    BEGIN:VEVENT\r\nUID:e\r\nDTSTAMP:20260101T000000Z\r\n\
                    DTSTART:20260101T000000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";
        let err = refusal(none, &["--summary", "x"]);
        assert!(err.contains("no VTODO"), "{err}");
    }

    fn at() -> Timestamp {
        "2026-10-28T08:30:00Z".parse().unwrap()
    }

    #[test]
    fn completing_stamps_the_completion_and_keeps_the_rest() {
        let done = String::from_utf8(complete(TASK.as_bytes(), at()).unwrap()).unwrap();

        assert_eq!(
            done,
            TASK.replace(
                "X-PIMALAYA-GTD;CONTEXT=office:next\r\n",
                "X-PIMALAYA-GTD;CONTEXT=office:next\r\nSTATUS:COMPLETED\r\n\
                 COMPLETED:20261028T083000Z\r\nPERCENT-COMPLETE:100\r\n"
            )
        );

        let reopened = String::from_utf8(reopen(done.as_bytes()).unwrap()).unwrap();
        assert_eq!(
            reopened,
            TASK.replace(
                "X-PIMALAYA-GTD;CONTEXT=office:next\r\n",
                "X-PIMALAYA-GTD;CONTEXT=office:next\r\nSTATUS:NEEDS-ACTION\r\n"
            )
        );
    }

    #[test]
    fn a_revision_stamps_the_task_and_its_last_modified_alone() {
        let revised = String::from_utf8(revise(TASK.as_bytes(), at()).unwrap()).unwrap();
        assert_eq!(
            revised,
            TASK.replace("DTSTAMP:20260101T000000Z", "DTSTAMP:20261028T083000Z")
        );

        let modified = TASK.replace("SUMMARY:", "LAST-MODIFIED:20260102T000000Z\r\nSUMMARY:");
        let revised = String::from_utf8(revise(modified.as_bytes(), at()).unwrap()).unwrap();
        assert!(
            revised.contains("LAST-MODIFIED:20261028T083000Z\r\n"),
            "{revised}"
        );

        assert_eq!(revise(b"not an ical", at()).unwrap(), b"not an ical");
    }
}
