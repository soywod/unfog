//! # JSON Schema registry
//!
//! Maps a CLI-invocation key, the command path joined with hyphens and
//! prefixed `unfog-`, to the JSON Schema of that command's `--json`
//! payload. [`JsonSchemaCommand`] writes one file per entry.
//!
//! Every data command appears, and so does every write: a `create`, an
//! `update`, a `done` or a `delete` answers the id it wrote, and an
//! abandoned edit is an outcome they report.
//!
//! Protocol-specific entries are gated behind the same cargo features as
//! their command modules, so the registry stays coherent under any
//! feature combination, none included.
//!
//! [`JsonSchemaCommand`]: pimalaya_cli::clap::commands::JsonSchemaCommand

use std::collections::BTreeMap;

use pimalaya_cli::{clap::commands::JsonSchemaRegistry, error::ErrorReport};
use schemars::schema_for;

use crate::error::ErrorCode;

/// Builds the command-to-schema registry consumed by `json-schema <DIR>`.
///
/// Each value describes the type the command hands to the printer, and
/// `unfog-error` the failure object every command prints instead.
pub fn registry() -> JsonSchemaRegistry {
    let mut schemas = BTreeMap::new();

    macro_rules! insert {
        ($key:expr, $ty:ty) => {
            schemas.insert(
                $key.to_string(),
                serde_json::to_value(schema_for!($ty)).unwrap(),
            );
        };
    }

    insert!("unfog-gtd-capture", crate::gtd::capture::CaptureOutput);
    for view in ["inbox", "next", "waiting", "someday"] {
        insert!(format!("unfog-gtd-{view}"), crate::gtd::view::GtdViewOutput);
    }
    insert!("unfog-gtd-clarify", crate::gtd::clarify::ClarifyOutput);
    insert!("unfog-gtd-review", crate::gtd::review::ReviewOutput);
    insert!("unfog-task-start", crate::time::track::StartOutput);
    insert!("unfog-task-stop", crate::time::track::StopOutput);
    insert!("unfog-task-status", crate::time::track::StatusOutput);
    insert!("unfog-worktime", crate::time::worktime::WorktimeOutput);
    insert!("unfog-migrate", crate::time::migrate::TimeMigrateOutput);
    insert!(
        "unfog-account-list",
        crate::account::list::AccountListOutput
    );
    insert!(
        "unfog-account-check",
        crate::account::check::AccountCheckOutput
    );
    insert!(
        "unfog-tasklist-list",
        crate::shared::tasklist::list::TasklistListOutput
    );
    insert!(
        "unfog-tasklist-create",
        crate::shared::tasklist::create::TasklistCreateOutput
    );
    insert!(
        "unfog-tasklist-delete",
        crate::shared::tasklist::delete::TasklistDeleteOutput
    );
    insert!("unfog-task-list", crate::shared::task::list::TaskListOutput);
    insert!("unfog-task-read", crate::shared::task::read::TaskReadOutput);
    insert!(
        "unfog-task-create",
        crate::shared::task::create::TaskCreateOutput
    );
    insert!(
        "unfog-task-update",
        crate::shared::task::update::TaskUpdateOutput
    );
    insert!("unfog-task-done", crate::shared::task::done::TaskDoneOutput);
    insert!(
        "unfog-task-undone",
        crate::shared::task::done::TaskDoneOutput
    );
    insert!(
        "unfog-task-delete",
        crate::shared::task::delete::TaskDeleteOutput
    );
    insert!("unfog-import", crate::import::ImportOutput);

    #[cfg(all(
        feature = "wizard",
        any(feature = "caldav", feature = "vdir", feature = "pimdir")
    ))]
    insert!("unfog-configure", crate::wizard::configure::ConfigureOutput);

    #[cfg(feature = "caldav")]
    insert!(
        "unfog-caldav-discover",
        crate::caldav::discover::CaldavDiscoverOutput
    );

    #[cfg(feature = "caldav")]
    insert!(
        "unfog-caldav-list",
        crate::caldav::list::CaldavCalendarListOutput
    );

    #[cfg(feature = "pimdir")]
    insert!(
        "unfog-pimdir-status",
        crate::pimdir::status::PimdirStatusOutput
    );

    #[cfg(feature = "vdir")]
    insert!(
        "unfog-vdir-list",
        crate::vdir::list::VdirCollectionListOutput
    );

    insert!("unfog-error", ErrorReport<ErrorCode>);

    JsonSchemaRegistry {
        bin: "unfog",
        version: env!("CARGO_PKG_VERSION"),
        schemas,
    }
}

#[cfg(test)]
mod tests {
    use crate::json_schema::registry;

    #[test]
    fn every_payload_is_an_object() {
        assert_eq!(registry().non_object_roots(), Vec::<&str>::new());
    }
}
