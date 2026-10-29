//! # GTD by categories
//!
//! The convention Pimalaya's task consumers share (pimdir GUIDE.md §17):
//! three reserved `CATEGORIES` values, `inbox`, `waiting` and `someday`,
//! written as those English literals and compared case-insensitively;
//! every other category is the person's own. A next action is an open
//! task carrying none of the three, open being any status but
//! `COMPLETED` and `CANCELLED`, a task stating no `STATUS` included.
//!
//! A writer edits only the three values: every other category stays
//! where it stood, and every line other than the `CATEGORIES` ones it
//! changes keeps its bytes.

use std::fmt;

use anyhow::{Result, bail};
use clap::ValueEnum;
use ical::{
    param::IcalParam,
    prop::{IcalProp, IcalPropKind, IcalPropName},
    tree::{codec::mode::Escaper, cst::IcalItem},
    value::{IcalValue, text::IcalTextList},
};
use schemars::JsonSchema;
use serde::Serialize;

use crate::shared::task::fields::{is_component, master_mut, names, one_vtodo, text};

/// A reserved state a task carries as one of its categories.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum GtdState {
    /// Captured, not yet clarified.
    Inbox,
    /// Someone else owes it.
    Waiting,
    /// Not a commitment.
    Someday,
}

impl GtdState {
    /// Every reserved state, in the order a review walks them.
    pub const ALL: [Self; 3] = [Self::Inbox, Self::Waiting, Self::Someday];

    /// The literal the state is written as, whatever the locale.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Inbox => "inbox",
            Self::Waiting => "waiting",
            Self::Someday => "someday",
        }
    }

    /// The state a category value stands for, compared as pimdir's
    /// `category:` compares: folded by the Unicode default lowercase
    /// mapping, so `Inbox` is `inbox`.
    pub fn of(category: &str) -> Option<Self> {
        let folded = fold(category);
        Self::ALL.into_iter().find(|state| state.as_str() == folded)
    }
}

/// A category folded for comparison (pimdir STORAGE Annex A.2's rule for
/// `fn`): lowercased, then trimmed.
pub fn fold(category: &str) -> String {
    category.to_lowercase().trim().to_owned()
}

/// Whether two categories are the same one.
pub fn same(a: &str, b: &str) -> bool {
    fold(a) == fold(b)
}

/// One of the four views over the tasks.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, JsonSchema, ValueEnum)]
#[serde(rename_all = "kebab-case")]
pub enum GtdView {
    /// Open tasks carrying `inbox`.
    Inbox,
    /// Open tasks carrying none of the reserved values.
    Next,
    /// Open tasks carrying `waiting`.
    Waiting,
    /// Open tasks carrying `someday`.
    Someday,
}

impl GtdView {
    /// The views in the order a review walks them.
    pub const ALL: [Self; 4] = [Self::Inbox, Self::Next, Self::Waiting, Self::Someday];

    /// Whether a task of `status` carrying `categories` is in the view,
    /// and carries every category of `filter`.
    ///
    /// The guide's queries, as unfog evaluates them:
    /// `kind:task -status:completed -status:cancelled` then
    /// `category:<state>` for a state, or the three negated for Next, a
    /// filter adding one `category:` term each.
    pub fn holds(self, status: Option<&str>, categories: &[String], filter: &[String]) -> bool {
        if !is_open(status) {
            return false;
        }

        let carries = |wanted: &str| categories.iter().any(|category| same(category, wanted));

        let state = match self {
            Self::Inbox => carries(GtdState::Inbox.as_str()),
            Self::Waiting => carries(GtdState::Waiting.as_str()),
            Self::Someday => carries(GtdState::Someday.as_str()),
            Self::Next => !GtdState::ALL.iter().any(|state| carries(state.as_str())),
        };

        state && filter.iter().all(|wanted| carries(wanted))
    }

    /// The query pimdir's guide spells the view as (GUIDE.md §17), a
    /// filter adding one `category:` term each.
    pub fn query(self, filter: &[String]) -> Result<String> {
        let mut query = String::from("kind:task -status:completed -status:cancelled");

        match self {
            Self::Inbox => query.push_str(" category:inbox"),
            Self::Waiting => query.push_str(" category:waiting"),
            Self::Someday => query.push_str(" category:someday"),
            Self::Next => query.push_str(" -category:inbox -category:waiting -category:someday"),
        }

        for category in filter {
            // NOTE: a phrase runs to the next double quote, with no
            // escape (pimdir SEARCH §8.3).
            if category.contains('"') {
                bail!("A category filter cannot hold a double quote: `{category}`");
            }
            query.push_str(&format!(" category:\"{category}\""));
        }

        Ok(query)
    }

    /// The view's name, as the command spells it.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Inbox => "inbox",
            Self::Next => "next",
            Self::Waiting => "waiting",
            Self::Someday => "someday",
        }
    }
}

impl fmt::Display for GtdView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Whether a task of `status` is open: any status but `COMPLETED` and
/// `CANCELLED`, a task stating none included.
pub fn is_open(status: Option<&str>) -> bool {
    !status.is_some_and(|status| {
        status.eq_ignore_ascii_case("COMPLETED") || status.eq_ignore_ascii_case("CANCELLED")
    })
}

/// Rewrites the categories of the VTODO of `ical`: its reserved values
/// replaced by `state` (`None` removing them), and the categories of
/// `add` it does not carry yet appended.
///
/// The state lands where the first reserved value stood, else on a new
/// `CATEGORIES` line after the last one (or after the task's last
/// property), which also takes what `add` appends. A `CATEGORIES` line
/// left holding nothing is removed. Every other line, and every
/// `CATEGORIES` line holding no reserved value, keeps its bytes.
pub fn edit_categories(ical: &[u8], state: Option<GtdState>, add: &[String]) -> Result<Vec<u8>> {
    let mut cst = one_vtodo(ical)?;
    let version = cst.version();
    let escaper = Escaper::for_version(version);

    let vtodo = master_mut(&mut cst)?;
    let items = std::mem::take(&mut vtodo.items);
    let mut kept = Vec::with_capacity(items.len());
    let mut placed = state.is_none();
    let mut carried: Vec<String> = Vec::new();

    for item in items {
        let IcalItem::Prop(line) = &item else {
            kept.push(item);
            continue;
        };

        if !names(&item, &IcalPropKind::Categories) {
            kept.push(item);
            continue;
        }

        let prop = line.decode(version);
        let values: Vec<String> = match &prop.value {
            IcalValue::TextList(list) => list.0.iter().map(|value| value.to_string()).collect(),
            IcalValue::Text(value) => vec![value.0.to_string()],
            _ => {
                kept.push(item);
                continue;
            }
        };

        if !values.iter().any(|value| GtdState::of(value).is_some()) {
            carried.extend(values);
            kept.push(item);
            continue;
        }

        let mut rewritten = Vec::with_capacity(values.len());

        for value in values {
            if GtdState::of(&value).is_none() {
                rewritten.push(value);
                continue;
            }

            if !placed && let Some(state) = state {
                rewritten.push(state.as_str().to_owned());
                placed = true;
            }
        }

        carried.extend(rewritten.iter().cloned());

        if rewritten.is_empty() {
            continue;
        }

        let prop = IcalProp {
            name: IcalPropName::Kind(IcalPropKind::Categories),
            params: prop.params,
            value: list(rewritten),
        };
        kept.push(IcalItem::Prop(prop.encode(escaper)));
    }

    let mut appended = Vec::new();

    if !placed && let Some(state) = state {
        appended.push(state.as_str().to_owned());
        carried.push(state.as_str().to_owned());
    }

    for category in add.iter().map(|category| category.trim()) {
        if category.is_empty() || carried.iter().any(|known| same(known, category)) {
            continue;
        }

        if GtdState::of(category).is_some() {
            bail!("`{category}` is a reserved category; clarify the task to set its state");
        }

        appended.push(category.to_owned());
        carried.push(category.to_owned());
    }

    if !appended.is_empty() {
        let prop = IcalProp {
            name: IcalPropName::Kind(IcalPropKind::Categories),
            params: Vec::new(),
            value: list(appended),
        };

        // NOTE: beside the categories the task already carries, else
        // after its last property, ahead of any nested component.
        let at = match kept
            .iter()
            .rposition(|item| names(item, &IcalPropKind::Categories))
        {
            Some(last) => last + 1,
            None => kept
                .iter()
                .position(|item| matches!(item, IcalItem::Component(_)))
                .unwrap_or(kept.len()),
        };

        kept.insert(at, IcalItem::Prop(prop.encode(escaper)));
    }

    vtodo.items = kept;

    Ok(cst.to_bytes())
}

/// Sets the GTD state of the VTODO of `ical` to `state` (`None` a next
/// action) at `now`, as pimdir GUIDE.md §17 writes it: `None` when the
/// task is in that state already, whatever the case it spells it in, so
/// nothing is written; else the state edited (see [`edit_categories`])
/// and the task revised.
pub fn set_state(
    ical: &[u8],
    state: Option<GtdState>,
    now: jiff::Timestamp,
) -> Result<Option<Vec<u8>>> {
    let task = crate::shared::item::TaskItem {
        contents: ical.to_vec(),
        ..Default::default()
    };
    let current = crate::shared::task::Task::project(&task)
        .first()
        .and_then(|task| {
            task.categories
                .iter()
                .find_map(|category| GtdState::of(category))
        });

    if current == state {
        return Ok(None);
    }

    let edited = edit_categories(ical, state, &[])?;
    Ok(Some(crate::shared::task::fields::revise(&edited, now)?))
}

/// Replaces the person's own categories of the VTODO of `ical` with
/// `categories`, keeping its GTD state (pimdir GUIDE.md §17): what a plain
/// `task update --category` writes.
///
/// The new categories land where the first of the person's own stood,
/// beside the reserved values of that line, else on a new `CATEGORIES`
/// line after the last one (or after the task's last property). The
/// person's other categories are dropped, a line left holding nothing
/// removed; a `CATEGORIES` line holding none of them keeps its bytes, as
/// does every other line. A reserved value is refused: a state is set by
/// clarifying.
pub fn own_categories(ical: &[u8], categories: &[String]) -> Result<Vec<u8>> {
    // NOTE: trimmed, the empty ones dropped, a value met again under
    // another case dropped too: the first spelling wins.
    let mut kept: Vec<String> = Vec::new();
    for category in categories.iter().map(|category| category.trim()) {
        if !category.is_empty() && !kept.iter().any(|known| same(known, category)) {
            kept.push(category.to_owned());
        }
    }
    let categories = kept;

    if let Some(reserved) = categories
        .iter()
        .find(|category| GtdState::of(category).is_some())
    {
        bail!("`{reserved}` is a reserved category; clarify the task to set its state");
    }

    let mut cst = one_vtodo(ical)?;
    let version = cst.version();
    let escaper = Escaper::for_version(version);

    let vtodo = master_mut(&mut cst)?;
    let items = std::mem::take(&mut vtodo.items);
    let mut kept = Vec::with_capacity(items.len());
    let mut placed = categories.is_empty();

    for item in items {
        let IcalItem::Prop(line) = &item else {
            kept.push(item);
            continue;
        };

        if !names(&item, &IcalPropKind::Categories) {
            kept.push(item);
            continue;
        }

        let prop = line.decode(version);
        let values: Vec<String> = match &prop.value {
            IcalValue::TextList(list) => list.0.iter().map(|value| value.to_string()).collect(),
            IcalValue::Text(value) => vec![value.0.to_string()],
            _ => {
                kept.push(item);
                continue;
            }
        };

        if values.iter().all(|value| GtdState::of(value).is_some()) {
            kept.push(item);
            continue;
        }

        let mut rewritten = Vec::with_capacity(values.len());

        for value in values {
            if GtdState::of(&value).is_some() {
                rewritten.push(value);
                continue;
            }

            if !placed {
                rewritten.extend(categories.iter().cloned());
                placed = true;
            }
        }

        if rewritten.is_empty() {
            continue;
        }

        let prop = IcalProp {
            name: IcalPropName::Kind(IcalPropKind::Categories),
            params: prop.params,
            value: list(rewritten),
        };
        kept.push(IcalItem::Prop(prop.encode(escaper)));
    }

    if !placed {
        let prop = IcalProp {
            name: IcalPropName::Kind(IcalPropKind::Categories),
            params: Vec::new(),
            value: list(categories),
        };

        let at = match kept
            .iter()
            .rposition(|item| names(item, &IcalPropKind::Categories))
        {
            Some(last) => last + 1,
            None => kept
                .iter()
                .position(|item| matches!(item, IcalItem::Component(_)))
                .unwrap_or(kept.len()),
        };

        kept.insert(at, IcalItem::Prop(prop.encode(escaper)));
    }

    vtodo.items = kept;

    Ok(cst.to_bytes())
}

/// Replaces every `CONTACT` of the VTODO of `ical` with `contact`, who a
/// waiting task waits on; `None` leaves them as they are.
///
/// A contact naming an e-mail address, bare or as `Name <address>`, is
/// written as the Pimalaya app writes it: the name (else the address) as
/// the value, the address as `ALTREP="mailto:..."`.
pub fn set_contact(ical: &[u8], contact: Option<&str>) -> Result<Vec<u8>> {
    let Some(contact) = contact.map(str::trim).filter(|contact| !contact.is_empty()) else {
        return Ok(ical.to_vec());
    };

    let mut cst = one_vtodo(ical)?;
    let escaper = Escaper::for_version(cst.version());
    let (name, address) = split_contact(contact);
    let mut prop = text(IcalPropKind::Contact, name.unwrap_or(contact));
    if let Some(address) = address {
        prop.params
            .push(IcalParam::AltRep(format!("mailto:{address}").into()));
    }
    let line = IcalItem::Prop(prop.encode(escaper));

    let vtodo = master_mut(&mut cst)?;
    let at = vtodo
        .items
        .iter()
        .position(|item| names(item, &IcalPropKind::Contact))
        .or_else(|| {
            vtodo
                .items
                .iter()
                .position(|item| is_component(item, "VALARM"))
        });

    vtodo
        .items
        .retain(|item| !names(item, &IcalPropKind::Contact));

    let at = at.unwrap_or(vtodo.items.len()).min(vtodo.items.len());
    vtodo.items.insert(at, line);

    Ok(cst.to_bytes())
}

/// The name and the e-mail address a contact spells: `Name <address>`,
/// a bare address, or a name alone.
fn split_contact(contact: &str) -> (Option<&str>, Option<&str>) {
    if let Some((name, rest)) = contact.split_once('<')
        && let Some((address, _)) = rest.split_once('>')
        && address.contains('@')
    {
        let name = name.trim().trim_matches('"').trim();
        return ((!name.is_empty()).then_some(name), Some(address.trim()));
    }

    match contact.contains('@') && !contact.contains(char::is_whitespace) {
        true => (Some(contact), Some(contact)),
        false => (Some(contact), None),
    }
}

/// A text list value.
fn list(values: Vec<String>) -> IcalValue<'static> {
    IcalValue::TextList(IcalTextList(values.into_iter().map(Into::into).collect()))
}

#[cfg(test)]
mod tests {
    use crate::shared::task::gtd::{
        GtdState, GtdView, edit_categories, own_categories, set_contact,
    };

    fn task(categories: &str) -> String {
        format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\nBEGIN:VTODO\r\n\
             UID:1@example.org\r\nDTSTAMP:20261028T080000Z\r\nSUMMARY:Call Jo\r\n\
             {categories}X-OTHER:kept\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        )
    }

    fn edit(categories: &str, state: Option<GtdState>, add: &[&str]) -> String {
        let add: Vec<String> = add.iter().map(|value| value.to_string()).collect();
        String::from_utf8(edit_categories(task(categories).as_bytes(), state, &add).unwrap())
            .unwrap()
    }

    #[test]
    fn a_cased_inbox_is_the_inbox() {
        assert_eq!(GtdState::of("Inbox"), Some(GtdState::Inbox));
        assert_eq!(GtdState::of(" SOMEDAY"), Some(GtdState::Someday));
        assert_eq!(GtdState::of("inboxes"), None);

        let categories = vec!["Inbox".to_owned(), "work".to_owned()];
        assert!(GtdView::Inbox.holds(None, &categories, &[]));
        assert!(!GtdView::Next.holds(None, &categories, &[]));
    }

    #[test]
    fn a_next_action_is_an_open_task_carrying_no_reserved_value() {
        let work = vec!["work".to_owned()];

        assert!(GtdView::Next.holds(None, &[], &[]));
        assert!(GtdView::Next.holds(Some("IN-PROCESS"), &work, &[]));
        assert!(GtdView::Next.holds(Some("NEEDS-ACTION"), &work, &["Work".into()]));
        assert!(!GtdView::Next.holds(Some("NEEDS-ACTION"), &work, &["home".into()]));
        assert!(!GtdView::Next.holds(Some("COMPLETED"), &work, &[]));
        assert!(!GtdView::Next.holds(Some("cancelled"), &work, &[]));

        let waiting = vec!["work".to_owned(), "WAITING".to_owned()];
        assert!(GtdView::Waiting.holds(Some("X-CUSTOM"), &waiting, &[]));
    }

    #[test]
    fn the_queries_are_the_guides() {
        assert_eq!(
            GtdView::Next.query(&["work".into()]).unwrap(),
            "kind:task -status:completed -status:cancelled -category:inbox -category:waiting \
             -category:someday category:\"work\""
        );
        assert_eq!(
            GtdView::Inbox.query(&[]).unwrap(),
            "kind:task -status:completed -status:cancelled category:inbox"
        );
        assert!(GtdView::Inbox.query(&["a\"b".into()]).is_err());
    }

    #[test]
    fn a_state_replaces_the_reserved_value_where_it_stood_and_keeps_the_rest() {
        let edited = edit(
            "CATEGORIES:work,Inbox,phone\r\nCATEGORIES:q3\r\n",
            Some(GtdState::Waiting),
            &[],
        );

        assert_eq!(
            edited,
            task("CATEGORIES:work,waiting,phone\r\nCATEGORIES:q3\r\n")
        );
    }

    #[test]
    fn clearing_the_state_keeps_every_other_category_in_order() {
        assert_eq!(
            edit("CATEGORIES:inbox\r\nCATEGORIES:work,home\r\n", None, &[]),
            task("CATEGORIES:work,home\r\n")
        );
        assert_eq!(
            edit("CATEGORIES:home,INBOX,work\r\n", None, &["phone"]),
            task("CATEGORIES:home,work\r\nCATEGORIES:phone\r\n")
        );
    }

    #[test]
    fn a_line_holding_no_reserved_value_keeps_its_bytes() {
        // NOTE: an escaped comma inside one value, which a re-encoding
        // would spell the same, and odd casing it would not.
        let original = "categories:Work\\, Q3,home\r\n";
        let edited = edit(original, Some(GtdState::Someday), &["Home", "errands"]);

        assert_eq!(
            edited,
            task("categories:Work\\, Q3,home\r\nCATEGORIES:someday,errands\r\n")
        );
    }

    #[test]
    fn a_reserved_value_is_refused_as_a_persons_category() {
        let add = vec!["Waiting".to_owned()];
        assert!(edit_categories(task("").as_bytes(), None, &add).is_err());
    }

    #[test]
    fn a_contact_replaces_every_contact() {
        let ical = task("CONTACT:Old\r\n");
        let edited = String::from_utf8(set_contact(ical.as_bytes(), Some("Jo")).unwrap()).unwrap();

        assert_eq!(edited, task("CONTACT:Jo\r\n"));
        assert_eq!(set_contact(ical.as_bytes(), None).unwrap(), ical.as_bytes());
    }

    #[test]
    fn a_contact_naming_an_address_carries_it_as_altrep() {
        let ical = task("CONTACT:Old\r\n");
        let contact = |value: &str| {
            String::from_utf8(set_contact(ical.as_bytes(), Some(value)).unwrap()).unwrap()
        };

        assert_eq!(
            contact("Jo Smith <jo@example.org>"),
            task("CONTACT;ALTREP=\"mailto:jo@example.org\":Jo Smith\r\n")
        );
        assert_eq!(
            contact("jo@example.org"),
            task("CONTACT;ALTREP=\"mailto:jo@example.org\":jo@example.org\r\n")
        );
        assert_eq!(contact("the plumber"), task("CONTACT:the plumber\r\n"));
    }

    fn own(categories: &str, new: &[&str]) -> String {
        let new: Vec<String> = new.iter().map(|value| value.to_string()).collect();
        String::from_utf8(own_categories(task(categories).as_bytes(), &new).unwrap()).unwrap()
    }

    /// A plain category edit replaces the person's categories and keeps
    /// the state where it stands, a line of reserved values alone kept.
    #[test]
    fn a_plain_category_edit_keeps_the_state() {
        assert_eq!(
            own("CATEGORIES:waiting,work\r\n", &["home"]),
            task("CATEGORIES:waiting,home\r\n")
        );
        assert_eq!(
            own(
                "CATEGORIES:Inbox\r\nCATEGORIES:a,b\r\nCATEGORIES:c\r\n",
                &["x", "y"]
            ),
            task("CATEGORIES:Inbox\r\nCATEGORIES:x,y\r\n")
        );
        assert_eq!(
            own("CATEGORIES:someday\r\n", &["home"]),
            task("CATEGORIES:someday\r\nCATEGORIES:home\r\n")
        );
        assert_eq!(
            own("CATEGORIES:waiting,work\r\n", &[""]),
            task("CATEGORIES:waiting\r\n")
        );

        let err = own_categories(task("").as_bytes(), &["Inbox".into()]).unwrap_err();
        assert!(err.to_string().contains("reserved"), "{err}");
    }
}
