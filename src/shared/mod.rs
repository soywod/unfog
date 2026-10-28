//! # Shared API
//!
//! The cross-protocol surface: unfog's own least-common-denominator
//! types, the dispatcher over the backends, and the command families built
//! on them.
//!
//! An operation only reaches this surface when every compiled backend can
//! serve it; what only one of them exposes lives in its protocol module.

pub mod arg;
pub mod client;
pub mod composer;
pub mod ical;
pub mod item;
pub mod table;
pub mod task;
pub mod tasklist;
pub mod uuid;
