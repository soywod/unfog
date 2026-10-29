//! # CalDAV backend
//!
//! The shared-API adapter over a connected [`WebdavClientStd`], whose
//! calendar home-set is already resolved. It projects RFC 4791
//! calendars and calendar object resources onto the shared types.
//!
//! Item ids are the resource names the server returned verbatim, so an
//! id a listing showed addresses the same resource on every verb. The
//! server owns the query, so the kind is pushed down as an RFC 4791
//! `comp-filter` rather than applied locally.

use std::collections::BTreeSet;

use anyhow::{Context, Result, bail};
use ical::{
    component::{IcalComponentKind, vtodo::VTODO},
    prop::uid::UID,
    tree::cst::IcalCst,
};
use io_webdav::{
    client::{WebdavClientStd, WebdavClientStdError},
    rfc4791::{calendar::CaldavCalendar, item::CaldavItemEntry},
    rfc4918::send::WebdavSendError,
};
use log::debug;
use pimalaya_config::secret::SecretResolver;

use crate::{
    caldav::client::connect,
    config::CaldavConfig,
    shared::{
        client::paginate,
        ical::holds_kind,
        item::{TaskItem, TaskItemQuery},
        tasklist::Tasklist,
    },
};

/// The shared-API glue over a connected CalDAV client.
pub struct CaldavBackend {
    client: WebdavClientStd,
    /// The calendars of the home, once listed.
    listed: Option<Vec<CaldavCalendar>>,
}

impl CaldavBackend {
    /// Connects and walks the discovery chain, caching the home-set.
    pub fn new(config: CaldavConfig) -> Result<Self> {
        Ok(Self {
            client: connect(&config, &mut SecretResolver::new())?,
            listed: None,
        })
    }

    /// The calendars of the home, listed once for the backend's life.
    fn listed(&mut self) -> Result<&[CaldavCalendar]> {
        if self.listed.is_none() {
            let calendars = self.client.list_calendars()?;
            self.listed = Some(calendars.into_iter().collect());
        }

        Ok(self.listed.as_deref().unwrap_or_default())
    }

    /// Lists the calendar collections under the calendar home-set that
    /// take VTODOs.
    ///
    /// An empty `supported-calendar-component-set` puts no restriction
    /// (RFC 4791 5.2.3), so such a calendar is kept.
    pub fn list_tasklists(&mut self) -> Result<Vec<Tasklist>> {
        let calendars = self.client.list_calendars()?;

        Ok(calendars
            .into_iter()
            .filter(takes_vtodo)
            .map(tasklist_from)
            .collect())
    }

    /// Creates a calendar collection through MKCALENDAR, its
    /// `supported-calendar-component-set` holding `VTODO` alone.
    pub fn create_tasklist(
        &mut self,
        id: &str,
        name: &str,
        description: Option<&str>,
        color: Option<&str>,
    ) -> Result<String> {
        let calendar = CaldavCalendar {
            id: id.to_owned(),
            display_name: Some(name.to_owned()),
            description: description.map(str::to_owned),
            color: color.map(str::to_owned),
            components: BTreeSet::from([String::from("VTODO")]),
            ..Default::default()
        };

        self.client.create_calendar(&calendar)?;
        Ok(id.to_owned())
    }

    /// Deletes a calendar collection and everything inside it.
    pub fn delete_tasklist(&mut self, id: &str) -> Result<()> {
        self.client.delete_calendar(id)?;
        Ok(())
    }

    /// Lists items through a `calendar-query` REPORT.
    ///
    /// The kind narrows the query server-side rather than locally.
    pub fn list_items(&mut self, tasklist_id: &str, query: TaskItemQuery) -> Result<Vec<TaskItem>> {
        let filter = comp_filter(&query);
        let entries = self.client.list_items(tasklist_id, &filter)?;

        let items = entries
            .into_iter()
            .map(|entry| item_from(tasklist_id, entry))
            .collect();

        Ok(paginate(items, query.page, query.page_size))
    }

    /// Reads one item's raw iCalendar bytes plus its entity tag.
    pub fn get_item(&mut self, tasklist_id: &str, item_id: &str) -> Result<TaskItem> {
        let body = self
            .client
            .read_item(tasklist_id, item_id)
            .with_context(|| format!("Read item `{item_id}` from calendar `{tasklist_id}`"))?;

        Ok(TaskItem {
            id: item_id.to_owned(),
            tasklist_id: tasklist_id.to_owned(),
            etag: body.etag,
            contents: body.data,
        })
    }

    /// PUTs a new resource.
    ///
    /// A server may name the resource itself, in which case the id it
    /// reported wins over the one asked for.
    pub fn create_item(&mut self, tasklist_id: &str, contents: Vec<u8>) -> Result<String> {
        self.refuse_unless_it_takes(tasklist_id, &contents)?;

        let id = format!("{}.ics", new_resource_name(&contents));
        let created = self.client.create_item(tasklist_id, &id, contents)?;
        Ok(created.id)
    }

    /// Moves the item `item_id` of `from` into `to` with a WebDAV `MOVE`,
    /// gated on `if_match`, then writes `contents` over it there, keeping
    /// its id.
    ///
    /// A server keeping one `UID` per account (Fastmail) refuses a create
    /// of the task in the target while the source still holds it, never a
    /// move. `None` when the server cannot move (Google answers 404), the
    /// caller then creating in the target and removing the source.
    pub fn move_item(
        &mut self,
        from: &str,
        item_id: &str,
        to: &str,
        contents: Vec<u8>,
        if_match: Option<&str>,
    ) -> Result<Option<String>> {
        self.refuse_unless_it_takes(to, &contents)?;

        let moved = match self.client.move_item(from, item_id, to, if_match) {
            Ok(moved) => moved,
            Err(WebdavClientStdError::Send(WebdavSendError::HttpStatus { status, .. }))
                if matches!(status, 404 | 405 | 501) =>
            {
                debug!("calendar `{from}` answered MOVE with {status}, moving by a copy");
                return Ok(None);
            }
            Err(err) => return Err(err.into()),
        };

        self.client
            .update_item(to, &moved.id, contents, moved.etag.as_deref())
            .with_context(|| {
                format!(
                    "The task was moved to calendar `{to}` as `{}`, but writing its new \
                     contents there failed; it holds the old ones",
                    moved.id
                )
            })?;

        Ok(Some(moved.id))
    }

    /// Refuses a task into a calendar stating it takes none, here where
    /// the reason can be said, rather than by the server.
    fn refuse_unless_it_takes(&mut self, tasklist_id: &str, contents: &[u8]) -> Result<()> {
        if holds_kind(contents, IcalComponentKind::VTodo)
            && let Some(calendar) = self
                .listed()?
                .iter()
                .find(|calendar| calendar.id == tasklist_id)
            && !takes_vtodo(calendar)
        {
            let takes: Vec<&str> = calendar.components.iter().map(String::as_str).collect();
            bail!(
                "Calendar `{tasklist_id}` takes no tasks (it takes {}); capture into a \
                 tasklist `unfog tasklist list` shows",
                takes.join(", ")
            );
        }

        Ok(())
    }

    /// PUTs over an existing resource, optionally gated on `if_match`.
    pub fn update_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        contents: Vec<u8>,
        if_match: Option<&str>,
    ) -> Result<()> {
        self.client
            .update_item(tasklist_id, item_id, contents, if_match)?;
        Ok(())
    }

    /// DELETEs a resource, gated on `if_match` when given.
    pub fn delete_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        if_match: Option<&str>,
    ) -> Result<()> {
        self.client.delete_item(tasklist_id, item_id, if_match)?;
        Ok(())
    }
}

/// The VCALENDAR child filter a listing sends, empty when it names no
/// kind.
fn comp_filter(query: &TaskItemQuery) -> String {
    match query.kind {
        Some(kind) => format!("<C:comp-filter name=\"{}\"></C:comp-filter>", &*kind),
        None => String::new(),
    }
}

/// Whether a calendar takes VTODOs: it names `VTODO` among its
/// components, or names none.
fn takes_vtodo(calendar: &CaldavCalendar) -> bool {
    calendar.components.is_empty()
        || calendar
            .components
            .iter()
            .any(|component| component.eq_ignore_ascii_case("VTODO"))
}

/// Projects a CalDAV calendar onto the shared [`Tasklist`].
///
/// The collection id stands in when the server sets no display name.
fn tasklist_from(calendar: CaldavCalendar) -> Tasklist {
    Tasklist {
        name: calendar
            .display_name
            .clone()
            .unwrap_or_else(|| calendar.id.clone()),
        id: calendar.id,
        description: calendar.description,
        color: calendar.color,
        default: false,
    }
}

/// Projects a CalDAV multistatus entry onto the shared [`TaskItem`].
fn item_from(tasklist_id: &str, entry: CaldavItemEntry) -> TaskItem {
    TaskItem {
        id: entry.id,
        tasklist_id: tasklist_id.to_owned(),
        etag: entry.etag,
        contents: entry.data,
    }
}

/// Derives the resource name a create PUTs to from the item's `UID`.
///
/// A content digest stands in when the bytes carry none. Only a
/// proposal: a server naming the resource itself reports its choice in
/// `Location`, which io-webdav returns instead.
fn new_resource_name(contents: &[u8]) -> String {
    let uid = IcalCst::parse(contents).ok().and_then(|cst| {
        cst.components::<VTODO>()
            .next()
            .and_then(|vtodo| vtodo.prop::<UID>())
            .map(|uid| uid.0.into_owned())
    });

    match uid {
        Some(uid) if is_safe_segment(&uid) => uid,
        _ => format!("{:016x}", fnv1a(contents)),
    }
}

/// Whether a `UID` can be a URL path segment verbatim.
///
/// No empty value, no separator, nothing needing percent-encoding.
fn is_safe_segment(uid: &str) -> bool {
    !uid.is_empty()
        && uid.len() <= 128
        && uid
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.' | '@'))
}

/// A 64-bit FNV-1a digest, the fallback name for a `UID`-less body.
fn fnv1a(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;

    for &byte in bytes {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }

    hash
}

#[cfg(test)]
mod tests {
    use ical::component::IcalComponentKind;

    use super::*;

    #[test]
    fn naming_no_kind_asks_for_every_component_kind() {
        assert_eq!(comp_filter(&TaskItemQuery::default()), "");
    }

    #[test]
    fn a_kind_narrows_the_query() {
        let query = TaskItemQuery {
            kind: Some(IcalComponentKind::VTodo),
            ..Default::default()
        };

        assert_eq!(
            comp_filter(&query),
            "<C:comp-filter name=\"VTODO\"></C:comp-filter>"
        );
    }

    #[test]
    fn a_calendar_naming_no_component_or_vtodo_takes_tasks() {
        let calendar = |components: &[&str]| CaldavCalendar {
            components: components.iter().map(|c| c.to_string()).collect(),
            ..Default::default()
        };

        assert!(takes_vtodo(&calendar(&[])));
        assert!(takes_vtodo(&calendar(&["VEVENT", "VTODO"])));
        assert!(!takes_vtodo(&calendar(&["VEVENT"])));
    }

    #[test]
    fn a_resource_name_comes_from_the_uid_when_it_is_url_safe() {
        let calendar = concat!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\n",
            "BEGIN:VTODO\r\nUID:task-1@example.org\r\nDTSTAMP:20260101T000000Z\r\n",
            "END:VTODO\r\nEND:VCALENDAR\r\n",
        );

        assert_eq!(new_resource_name(calendar.as_bytes()), "task-1@example.org");
    }

    #[test]
    fn an_unsafe_or_missing_uid_falls_back_to_a_digest() {
        let slashed = concat!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//x//y//EN\r\n",
            "BEGIN:VTODO\r\nUID:a/b\r\nDTSTAMP:20260101T000000Z\r\n",
            "END:VTODO\r\nEND:VCALENDAR\r\n",
        );

        let name = new_resource_name(slashed.as_bytes());
        assert_eq!(name.len(), 16);
        assert!(name.chars().all(|c| c.is_ascii_hexdigit()));

        assert_eq!(name, new_resource_name(slashed.as_bytes()));
        assert_ne!(name, new_resource_name(b"other bytes entirely"));
    }
}
