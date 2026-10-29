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

use std::{collections::BTreeSet, io::ErrorKind};

use anyhow::{Context, Result, bail};
use ical::{
    component::{IcalComponentKind, vtodo::VTODO},
    prop::uid::UID,
    tree::cst::IcalCst,
};
use io_webdav::{
    client::WebdavClientStd,
    rfc4791::{calendar::CaldavCalendar, item::CaldavItemEntry},
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
    /// The configuration it connected from, to connect again.
    config: CaldavConfig,
    /// The calendars of the home, once listed.
    listed: Option<Vec<CaldavCalendar>>,
}

impl CaldavBackend {
    /// Connects and walks the discovery chain, caching the home-set.
    pub fn new(config: CaldavConfig) -> Result<Self> {
        Ok(Self {
            client: connect(&config, &mut SecretResolver::new())?,
            config,
            listed: None,
        })
    }

    /// Runs `op` over the connection, once more over a new one when the
    /// server had closed it.
    ///
    /// The client keeps one connection for the run, while a server may
    /// answer `Connection: close` (Radicale behind waitress does after a
    /// PUT) or drop an idle one: the next request then meets the end of
    /// the stream before any response, so it was never processed and is
    /// sent again over a fresh connection to the cached home-set.
    fn send<T, E>(&mut self, mut op: impl FnMut(&mut WebdavClientStd) -> Result<T, E>) -> Result<T>
    where
        E: Into<anyhow::Error>,
    {
        let err = match op(&mut self.client) {
            Ok(value) => return Ok(value),
            Err(err) => err.into(),
        };

        if !is_dropped(&err) {
            return Err(err);
        }

        debug!("the CalDAV connection was closed, connecting again: {err:#}");
        let mut config = self.config.clone();
        if let Some(home) = self.client.calendar_home_set.clone() {
            config.home = Some(home);
        }
        self.client = connect(&config, &mut SecretResolver::new())?;

        op(&mut self.client).map_err(Into::into)
    }

    /// The calendars of the home, listed once for the backend's life.
    fn listed(&mut self) -> Result<&[CaldavCalendar]> {
        if self.listed.is_none() {
            let calendars = self.send(|client| client.list_calendars())?;
            self.listed = Some(calendars.into_iter().collect());
        }

        Ok(self.listed.as_deref().unwrap_or_default())
    }

    /// Refuses a calendar id naming several calendars of the home.
    ///
    /// A calendar is addressed as `<home>/<id>`, its id the last segment of
    /// its href. A server keeping calendars outside the home (Google, each
    /// at `/caldav/v2/<calendar>/events/`) lists them all under one id,
    /// which then names the home's own calendar, not the one listed: a
    /// write would land in another calendar than the one asked for.
    fn addressable(&mut self, tasklist_id: &str) -> Result<()> {
        let listed = self.listed()?;

        if ambiguous(listed).iter().any(|id| id == tasklist_id) {
            bail!(
                "Calendar id `{tasklist_id}` names several calendars of the CalDAV home (the \
                 server keeps them outside it, as Google does), so it addresses none of them \
                 reliably; nothing was read or written"
            );
        }

        Ok(())
    }

    /// Lists the calendar collections under the calendar home-set that
    /// take VTODOs.
    ///
    /// An empty `supported-calendar-component-set` puts no restriction
    /// (RFC 4791 5.2.3), so such a calendar is kept.
    pub fn list_tasklists(&mut self) -> Result<Vec<Tasklist>> {
        let calendars = self.send(|client| client.list_calendars())?;

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

        self.send(|client| client.create_calendar(&calendar))?;
        Ok(id.to_owned())
    }

    /// Deletes a calendar collection and everything inside it.
    pub fn delete_tasklist(&mut self, id: &str) -> Result<()> {
        self.send(|client| client.delete_calendar(id))?;
        Ok(())
    }

    /// Lists items through a `calendar-query` REPORT.
    ///
    /// The kind narrows the query server-side rather than locally.
    pub fn list_items(&mut self, tasklist_id: &str, query: TaskItemQuery) -> Result<Vec<TaskItem>> {
        self.addressable(tasklist_id)?;
        let filter = comp_filter(&query);
        let entries = self.send(|client| client.list_items(tasklist_id, &filter))?;

        let items = entries
            .into_iter()
            .map(|entry| item_from(tasklist_id, entry))
            .collect();

        Ok(paginate(items, query.page, query.page_size))
    }

    /// Reads one item's raw iCalendar bytes plus its entity tag.
    pub fn get_item(&mut self, tasklist_id: &str, item_id: &str) -> Result<TaskItem> {
        self.addressable(tasklist_id)?;
        let body = self
            .send(|client| client.read_item(tasklist_id, item_id))
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
        self.addressable(tasklist_id)?;

        // NOTE: a task into a calendar stating it takes none is refused
        // here, where the reason can be said, rather than by the server.
        if holds_kind(&contents, IcalComponentKind::VTodo)
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

        let id = format!("{}.ics", new_resource_name(&contents));
        let created = self.send(|client| client.create_item(tasklist_id, &id, contents.clone()))?;
        Ok(created.id)
    }

    /// PUTs over an existing resource, optionally gated on `if_match`.
    pub fn update_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        contents: Vec<u8>,
        if_match: Option<&str>,
    ) -> Result<()> {
        self.addressable(tasklist_id)?;
        self.send(|client| client.update_item(tasklist_id, item_id, contents.clone(), if_match))?;
        Ok(())
    }

    /// DELETEs a resource, gated on `if_match` when given.
    pub fn delete_item(
        &mut self,
        tasklist_id: &str,
        item_id: &str,
        if_match: Option<&str>,
    ) -> Result<()> {
        self.addressable(tasklist_id)?;
        self.send(|client| client.delete_item(tasklist_id, item_id, if_match))?;
        Ok(())
    }
}

/// The ids several of `calendars` share.
fn ambiguous<'a>(calendars: impl IntoIterator<Item = &'a CaldavCalendar>) -> Vec<String> {
    let mut ids: Vec<&str> = calendars
        .into_iter()
        .map(|calendar| calendar.id.as_str())
        .collect();
    ids.sort_unstable();

    let mut shared: Vec<String> = ids
        .windows(2)
        .filter(|pair| pair[0] == pair[1])
        .map(|pair| pair[0].to_owned())
        .collect();
    shared.dedup();
    shared
}

/// Whether `err` is a connection the server closed: the stream ended, or
/// was reset or broken, before a response.
fn is_dropped(err: &anyhow::Error) -> bool {
    err.chain().any(|cause| {
        if let Some(io) = cause.downcast_ref::<std::io::Error>() {
            return matches!(
                io.kind(),
                ErrorKind::UnexpectedEof
                    | ErrorKind::BrokenPipe
                    | ErrorKind::ConnectionReset
                    | ErrorKind::ConnectionAborted
            );
        }

        // NOTE: the HTTP layer reports these as text alone.
        let text = cause.to_string();
        ["unexpected EOF", "Connection reset by peer", "Broken pipe"]
            .iter()
            .any(|dropped| text.contains(dropped))
    })
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

    /// Google lists every calendar under the id `events`.
    #[test]
    fn an_id_several_calendars_share_is_ambiguous() {
        let calendar = |id: &str| CaldavCalendar {
            id: id.into(),
            ..Default::default()
        };
        let listed = [calendar("events"), calendar("tasks"), calendar("events")];

        assert_eq!(ambiguous(&listed), ["events"]);
        assert!(ambiguous(&listed[..2]).is_empty());
    }

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
