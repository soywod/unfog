//! # Review
//!
//! The `review` command: the weekly review as the four views, counts
//! first.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    gtd::view::{GtdViewArgs, GtdViewOutput, view},
    scope::Scope,
    shared::task::gtd::GtdView,
};

/// Walk the weekly review: how many tasks each view holds, then the
/// Inbox, Next, Waiting and Someday in turn.
///
/// JSON output: `{"counts": {"inbox", "next", "waiting", "someday"},
/// "views": [<the output of each view>]}`.
#[derive(Debug, Parser)]
pub struct ReviewCommand {
    #[command(flatten)]
    pub args: GtdViewArgs,
}

impl ReviewCommand {
    pub fn execute(self, printer: &mut impl Printer, mut scope: Scope) -> Result<()> {
        let mut views = Vec::with_capacity(GtdView::ALL.len());

        for kind in GtdView::ALL {
            views.push(view(&mut scope, kind, &[], self.args.max_width)?);
        }

        let count = |kind: GtdView| {
            views
                .iter()
                .find(|view| view.view == kind)
                .map_or(0, |view| view.tasks.len())
        };

        printer.out(ReviewOutput {
            counts: ReviewCounts {
                inbox: count(GtdView::Inbox),
                next: count(GtdView::Next),
                waiting: count(GtdView::Waiting),
                someday: count(GtdView::Someday),
            },
            views,
        })
    }
}

/// How many tasks each view holds.
#[derive(Clone, Copy, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ReviewCounts {
    /// Tasks to clarify.
    pub inbox: usize,
    /// Next actions.
    pub next: usize,
    /// Tasks waiting on someone.
    pub waiting: usize,
    /// Tasks for Someday.
    pub someday: usize,
}

/// The weekly review.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ReviewOutput {
    /// How many tasks each view holds.
    pub counts: ReviewCounts,
    /// The views, in the order a review walks them.
    pub views: Vec<GtdViewOutput>,
}

impl fmt::Display for ReviewOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let counts = self.counts;
        writeln!(
            f,
            "Inbox {}, Next {}, Waiting {}, Someday {}",
            counts.inbox, counts.next, counts.waiting, counts.someday
        )?;

        for view in &self.views {
            let title = match view.view {
                GtdView::Inbox => "Inbox: clarify each one (unfog gtd clarify)",
                GtdView::Next => "Next: still the next actions?",
                GtdView::Waiting => "Waiting: anyone to chase?",
                GtdView::Someday => "Someday: anything to start now?",
            };

            writeln!(f)?;
            writeln!(f, "{title}")?;

            match view.tasks.is_empty() {
                true => writeln!(f, "  (none)")?,
                false => write!(f, "{view}")?,
            }
        }

        Ok(())
    }
}
