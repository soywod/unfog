//! # Review
//!
//! The `review` command: the weekly review as the four views, counts
//! first, then the waiting tasks to chase.

use std::fmt;

use anyhow::Result;
use clap::Parser;
use jiff::Timestamp;
use pimalaya_cli::printer::Printer;
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    gtd::view::{GtdViewArgs, GtdViewOutput, GtdViewRow, view_at},
    scope::Scope,
    shared::task::gtd::GtdView,
};

/// How many days a task waits before the review asks to chase it.
const LONG_WAIT_DAYS: i64 = 14;

/// Walk the weekly review: how many tasks each view holds, then the
/// Inbox, Next, Waiting and Someday in turn, then the waiting tasks
/// waiting over two weeks and those whose message got a reply.
///
/// The scope is listed once for the whole review. A reply is found on
/// pimdir, where a task captured from a message (`ATTACH:mid:`) meets a
/// mail of the store replying to it.
///
/// JSON output: `{"counts": {"inbox", "next", "waiting", "someday"},
/// "views": [<the output of each view>], "waitingLong": [...],
/// "replied": [...]}`, the last two rows of the Waiting view.
#[derive(Debug, Parser)]
pub struct ReviewCommand {
    #[command(flatten)]
    pub args: GtdViewArgs,
}

impl ReviewCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Reviews at `now`, the instant waiting is counted to.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        let mut views = Vec::with_capacity(GtdView::ALL.len());

        for kind in GtdView::ALL {
            views.push(view_at(&mut scope, kind, &[], self.args.max_width, now)?);
        }

        let count = |kind: GtdView| {
            views
                .iter()
                .find(|view| view.view == kind)
                .map_or(0, |view| view.tasks.len())
        };

        let waiting: Vec<GtdViewRow> = views
            .iter()
            .find(|view| view.view == GtdView::Waiting)
            .map(|view| view.tasks.clone())
            .unwrap_or_default();

        let waiting_long = waiting
            .iter()
            .filter(|row| row.waited_days.is_some_and(|days| days >= LONG_WAIT_DAYS))
            .cloned()
            .collect();

        let replied = replied(&mut scope, &waiting)?;

        printer.out(ReviewOutput {
            counts: ReviewCounts {
                inbox: count(GtdView::Inbox),
                next: count(GtdView::Next),
                waiting: count(GtdView::Waiting),
                someday: count(GtdView::Someday),
            },
            views,
            waiting_long,
            replied,
        })
    }
}

/// The waiting rows whose message got a reply, asked of each account
/// holding one.
fn replied(scope: &mut Scope, waiting: &[GtdViewRow]) -> Result<Vec<GtdViewRow>> {
    let mut replied = Vec::new();

    for index in 0..scope.accounts.len() {
        let name = scope.name(index).to_owned();
        let rows: Vec<&GtdViewRow> = waiting
            .iter()
            .filter(|row| row.account == name && row.task.mid.is_some())
            .collect();
        let mids: Vec<String> = rows.iter().filter_map(|row| row.task.mid.clone()).collect();

        let answered = scope.client(index).replied(&mids)?;

        replied.extend(
            rows.into_iter()
                .filter(|row| {
                    row.task
                        .mid
                        .as_ref()
                        .is_some_and(|mid| answered.contains(mid))
                })
                .cloned(),
        );
    }

    Ok(replied)
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
    /// The waiting tasks waiting over two weeks.
    pub waiting_long: Vec<GtdViewRow>,
    /// The waiting tasks whose message got a reply.
    pub replied: Vec<GtdViewRow>,
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

        let sections = [
            ("Waiting over two weeks: chase them?", &self.waiting_long),
            ("A reply came: clarify them again?", &self.replied),
        ];

        for (title, rows) in sections {
            if rows.is_empty() {
                continue;
            }

            writeln!(f)?;
            writeln!(f, "{title}")?;

            for row in rows {
                let contacts = match row.task.contacts.is_empty() {
                    true => String::new(),
                    false => format!(" ({})", row.task.contacts.join(", ")),
                };
                writeln!(f, "  {} {}{contacts}", row.short_id, row.task.summary)?;
            }
        }

        Ok(())
    }
}
