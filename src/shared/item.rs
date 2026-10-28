//! # Item
//!
//! The iCalendar object resource shared by every backend: the raw bytes a
//! task is projected from.
//!
//! [`TaskItem`] is the raw view: an id, an optional entity tag, and the bytes
//! verbatim. unfog never rewrites bytes it was not asked to change, so what a
//! backend stored is what a read returns.

use ical::component::IcalComponentKind;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// A calendar object resource: one iCalendar file.
///
/// A collection may mix component kinds (VTODO, VEVENT, VJOURNAL): the
/// task commands keep the VTODOs.
#[derive(Clone, Debug, Default, Eq, PartialEq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TaskItem {
    /// Backend-specific identifier the other commands address it by.
    pub id: String,
    /// The tasklist the item lives in.
    pub tasklist_id: String,
    /// Entity tag, for the backends offering optimistic concurrency.
    #[serde(default)]
    pub etag: Option<String>,
    /// Raw iCalendar bytes, exactly as the backend stored them.
    #[serde(default)]
    pub contents: Vec<u8>,
}

/// What narrows an item listing, and in which order.
///
/// The kind is applied before the page, so a page of tasks is a page of
/// VTODOs rather than the VTODOs among a page of any kind.
#[derive(Clone, Copy, Debug, Default)]
pub struct TaskItemQuery {
    /// 1-indexed page, `None` being the first.
    pub page: Option<u32>,
    /// Items per page, `None` returning the whole listing.
    pub page_size: Option<u32>,
    /// Component kind to keep, `None` keeping every kind.
    pub kind: Option<IcalComponentKind>,
}
