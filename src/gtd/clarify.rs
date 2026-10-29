//! # Clarify
//!
//! The `clarify` command: what a captured task is, decided one task at a
//! time, as the Pimalaya app's guided flow does.
//!
//! A result is an edit of the same item, guarded by the version it was
//! read at. Moving it to another tasklist is the one exception: it is
//! created in the target, and removed from the source once the create
//! was accepted, so a refused create keeps the capture.

use core::fmt;
use std::io::{IsTerminal, stdin};

use anyhow::{Context, Result, bail};
use clap::{ArgGroup, Parser};
use jiff::Timestamp;
use pimalaya_cli::{printer::Printer, prompt};
use schemars::JsonSchema;
use serde::Serialize;

use crate::{
    scope::{Located, Scope},
    shared::task::{
        fields::{TaskFieldsArgs, complete, revise},
        gtd::{GtdState, GtdView, edit_categories, set_contact, set_state},
        list::sort_key,
    },
};

/// Clarify what a task is, the Inbox one task at a time when none is
/// named.
///
/// Interactively, each task is rephrased, then: not actionable, it is
/// deleted or kept for Someday; done in under two minutes, it is marked
/// done; someone else's, it waits on them; otherwise it is a next
/// action, given categories (contexts, a project), a due date and
/// optionally another tasklist.
///
/// Each outcome is also a flag, for scripts: `unfog gtd clarify 4c1e
/// --summary "Call Jo about the lease" --next -C phone`.
/// An outcome replaces the reserved categories (`inbox`, `waiting`,
/// `someday`) and adds the ones given, every other category and every
/// other line of the task kept.
///
/// JSON output: `{"clarified": [{"account", "tasklist", "id", "uid",
/// "outcome", "moved"}]}`; the interactive flow refuses `--json`.
#[derive(Debug, Parser)]
#[command(group(
    ArgGroup::new("outcome").args(["delete", "someday", "done", "waiting", "next"])
))]
pub struct ClarifyCommand {
    /// Task to clarify: its handle, or a prefix of its UID. Omit to walk
    /// the Inbox.
    #[arg(value_name = "TASK")]
    pub task: Option<String>,
    /// Rephrase the task (`SUMMARY`).
    #[arg(long, value_name = "TEXT")]
    pub summary: Option<String>,
    /// Not actionable, and not worth keeping: delete it.
    #[arg(long)]
    pub delete: bool,
    /// Not a commitment yet: keep it for Someday.
    #[arg(long)]
    pub someday: bool,
    /// Done already, in under two minutes.
    #[arg(long)]
    pub done: bool,
    /// Someone else owes it: wait on them.
    #[arg(long)]
    pub waiting: bool,
    /// A next action.
    #[arg(long)]
    pub next: bool,
    /// Who a waiting task waits on (`CONTACT`).
    #[arg(long, value_name = "TEXT")]
    pub contact: Option<String>,
    /// A category to add (a context, a project), the flag repeating.
    #[arg(long = "category", short = 'C', value_name = "TEXT")]
    pub categories: Vec<String>,
    /// When the task is due (`DUE`), spelled as `task update --due` is.
    #[arg(long, value_name = "TIME")]
    pub due: Option<String>,
    /// Priority of the task (`PRIORITY`), 1 the highest, 9 the lowest, 0
    /// undefined.
    #[arg(long, value_name = "0-9", value_parser = clap::value_parser!(u8).range(0..=9))]
    pub priority: Option<u8>,
    /// The task this one is a step of (`RELATED-TO`, the parent): its
    /// handle, or a prefix of its UID.
    #[arg(long, value_name = "TASK")]
    pub parent: Option<String>,
    /// Move the task to this tasklist.
    #[arg(long = "to", value_name = "TASKLIST")]
    pub to: Option<String>,
    /// The account of the `--to` tasklist, the task's own by default.
    #[arg(long = "to-account", value_name = "ACCOUNT", requires = "to")]
    pub to_account: Option<String>,
}

/// What a task is, once clarified.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum ClarifyOutcome {
    /// Deleted.
    Delete,
    /// Kept for Someday.
    Someday,
    /// Marked done.
    Done,
    /// Waiting on someone else.
    Waiting,
    /// A next action.
    Next,
}

/// One clarification: the outcome and what comes with it.
#[derive(Clone, Debug)]
pub struct Clarification {
    /// The new summary.
    pub summary: Option<String>,
    /// What the task is.
    pub outcome: ClarifyOutcome,
    /// Who a waiting task waits on.
    pub contact: Option<String>,
    /// Categories to add.
    pub categories: Vec<String>,
    /// When it is due.
    pub due: Option<String>,
    /// The task it is a step of, as named on the command line.
    pub parent: Option<String>,
    /// Its priority.
    pub priority: Option<u8>,
    /// The tasklist to move it to, in the account named, the task's own
    /// when `None`.
    pub to: Option<(Option<String>, String)>,
}

impl ClarifyCommand {
    pub fn execute(self, printer: &mut impl Printer, scope: Scope) -> Result<()> {
        self.execute_at(printer, scope, Timestamp::now())
    }

    /// Clarifies, `now` the instant the tasks are revised at.
    pub fn execute_at(
        self,
        printer: &mut impl Printer,
        mut scope: Scope,
        now: Timestamp,
    ) -> Result<()> {
        let to = self
            .to
            .clone()
            .map(|tasklist| (self.to_account.clone(), tasklist));

        let outcome = match (
            self.delete,
            self.someday,
            self.done,
            self.waiting,
            self.next,
        ) {
            (true, ..) => Some(ClarifyOutcome::Delete),
            (_, true, ..) => Some(ClarifyOutcome::Someday),
            (_, _, true, ..) => Some(ClarifyOutcome::Done),
            (.., true, _) => Some(ClarifyOutcome::Waiting),
            (.., true) => Some(ClarifyOutcome::Next),
            _ => None,
        };

        // NOTE: clap counts an outcome flag left false as present, so
        // `requires` cannot say this.
        if self.contact.is_some() && outcome != Some(ClarifyOutcome::Waiting) {
            bail!("--contact names who a task waits on; give it with --waiting");
        }

        if let Some(outcome) = outcome {
            let Some(task) = &self.task else {
                bail!("Name the task to clarify as an outcome flag says");
            };

            let found = scope.find(task)?;
            let clarification = Clarification {
                summary: self.summary,
                outcome,
                contact: self.contact,
                categories: self.categories,
                due: self.due,
                priority: self.priority,
                parent: self.parent,
                to,
            };

            let clarified = apply(&mut scope, &found, &clarification, now)?;
            return printer.out(ClarifyOutput {
                clarified: vec![clarified],
            });
        }

        if printer.is_json() || !stdin().is_terminal() {
            bail!(
                "Clarifying interactively needs a terminal; name a task and an outcome flag \
                 (--delete, --someday, --done, --waiting or --next)"
            );
        }

        let mut queue = match &self.task {
            Some(task) => vec![scope.find(task)?],
            None => {
                scope.select(&|status, categories| GtdView::Inbox.holds(status, categories, &[]))?
            }
        };
        queue.sort_by_cached_key(|found| sort_key(&found.task));

        if queue.is_empty() {
            eprintln!("The Inbox is empty");
        }

        // NOTE: the tasklists a next action may go to, listed once for
        // the whole walk.
        let mut targets: Vec<(usize, String)> = Vec::new();
        for index in 0..scope.accounts.len() {
            if scope.accounts[index].extra {
                continue;
            }

            for tasklist in scope.client(index).list_tasklists()? {
                targets.push((index, tasklist.id));
            }
        }

        let mut clarified = Vec::new();
        let total = queue.len();

        for (index, found) in queue.iter().enumerate() {
            eprintln!();
            eprintln!("({}/{total}) {}", index + 1, found.task.summary);

            let Some(clarification) = ask(&mut scope, found, &targets)? else {
                continue;
            };

            clarified.push(apply(&mut scope, found, &clarification, now)?);
        }

        printer.out(ClarifyOutput { clarified })
    }
}

/// Asks what a task is, `None` when it is skipped.
fn ask(
    scope: &mut Scope,
    found: &Located,
    targets: &[(usize, String)],
) -> Result<Option<Clarification>> {
    let summary = prompt::text("Rephrase it:", Some(found.task.summary.as_str()))?;
    let summary = (summary.trim() != found.task.summary).then_some(summary);

    let mut clarification = Clarification {
        summary,
        outcome: ClarifyOutcome::Next,
        contact: None,
        categories: Vec::new(),
        due: None,
        priority: None,
        parent: None,
        to: None,
    };

    if !prompt::bool("Is it actionable?", true)? {
        let choice = prompt::item(
            "Then:",
            ["Keep it for Someday", "Delete it", "Skip it for now"],
            None,
        )?;

        clarification.outcome = match choice {
            "Keep it for Someday" => ClarifyOutcome::Someday,
            "Delete it" => ClarifyOutcome::Delete,
            _ => return Ok(None),
        };

        return Ok(Some(clarification));
    }

    if prompt::bool("Under two minutes? Do it now, then mark it done", false)? {
        clarification.outcome = ClarifyOutcome::Done;
        return Ok(Some(clarification));
    }

    if prompt::bool("Is someone else doing it?", false)? {
        clarification.outcome = ClarifyOutcome::Waiting;
        clarification.contact = prompt::some_text("Waiting on whom (optional):", None)?;
        return Ok(Some(clarification));
    }

    if let Some(categories) =
        prompt::some_text("Categories, comma-separated (contexts, a project):", None)?
    {
        clarification.categories = categories
            .split(',')
            .map(|category| category.trim().to_owned())
            .filter(|category| !category.is_empty())
            .collect();
    }

    clarification.due = prompt::some_text("Due (YYYY-MM-DD, optional):", None)?
        .filter(|due| !due.trim().is_empty());

    // NOTE: a parent is checked now, so a mistyped one is asked again
    // rather than failing the write.
    while let Some(parent) = prompt::some_text("Step of which task (an id, optional):", None)? {
        let parent = parent.trim();
        if parent.is_empty() {
            break;
        }

        match scope.find(parent) {
            Ok(task) => {
                clarification.parent = Some(task.task.uid);
                break;
            }
            Err(err) => eprintln!("{err}"),
        }
    }

    if targets.len() > 1 {
        let label = |(index, tasklist): &(usize, String)| match scope.is_multi() {
            true => format!("{tasklist} ({})", scope.name(*index)),
            false => tasklist.clone(),
        };
        let labels: Vec<String> = targets.iter().map(label).collect();
        let current = label(&(found.account, found.tasklist.clone()));

        let chosen = prompt::item("Tasklist:", labels.clone(), Some(current))?;
        let at = labels.iter().position(|label| *label == chosen);

        if let Some((index, tasklist)) = at.map(|at| targets[at].clone()) {
            clarification.to = Some((Some(scope.name(index).to_owned()), tasklist));
        }
    }

    Ok(Some(clarification))
}

/// Applies a clarification to a located task, `now` the instant it is
/// revised at.
pub fn apply(
    scope: &mut Scope,
    found: &Located,
    clarification: &Clarification,
    now: Timestamp,
) -> Result<ClarifiedTask> {
    let item = scope.item(found)?;
    let etag = item.etag.clone();
    let outcome = clarification.outcome;

    let target = match &clarification.to {
        None => None,
        Some(_) if matches!(outcome, ClarifyOutcome::Delete | ClarifyOutcome::Done) => {
            bail!("A deleted or done task moves nowhere; drop --to")
        }
        Some((account, tasklist)) => {
            let account = match account {
                Some(name) => scope.index_of(name)?,
                None => found.account,
            };

            (account != found.account || *tasklist != found.tasklist)
                .then(|| (account, tasklist.clone()))
        }
    };

    let mut clarified = ClarifiedTask {
        account: scope.name(found.account).to_owned(),
        tasklist: found.tasklist.clone(),
        id: found.task.id.clone(),
        uid: found.task.uid.clone(),
        outcome,
        moved: false,
        unchanged: false,
    };

    if outcome == ClarifyOutcome::Delete {
        scope.client(found.account).delete_item(
            &found.tasklist,
            &found.task.id,
            etag.as_deref(),
        )?;
        return Ok(clarified);
    }

    let nothing_else = clarification.summary.is_none()
        && clarification.categories.is_empty()
        && clarification.due.is_none()
        && clarification.priority.is_none()
        && clarification.parent.is_none()
        && target.is_none();

    // NOTE: a state alone is pimdir GUIDE.md §17's set-state writer, which
    // leaves a task already in that state untouched: no write, no new
    // DTSTAMP.
    let state = match outcome {
        ClarifyOutcome::Someday => Some(Some(GtdState::Someday)),
        ClarifyOutcome::Waiting if clarification.contact.is_none() => Some(Some(GtdState::Waiting)),
        ClarifyOutcome::Next => Some(None),
        _ => None,
    };

    if nothing_else && let Some(state) = state {
        match set_state(&item.contents, state, now)? {
            None => clarified.unchanged = true,
            Some(contents) => scope.client(found.account).update_item(
                &found.tasklist,
                &found.task.id,
                contents,
                etag.as_deref(),
            )?,
        }
        return Ok(clarified);
    }

    let done = outcome == ClarifyOutcome::Done
        && found.task.status.as_deref() == Some("COMPLETED")
        && !found
            .task
            .categories
            .iter()
            .any(|category| GtdState::of(category).is_some());

    if nothing_else && done {
        clarified.unchanged = true;
        return Ok(clarified);
    }

    let parent = match &clarification.parent {
        Some(query) => Some(scope.find(query)?.task.uid),
        None => None,
    };

    let fields = TaskFieldsArgs {
        summary: clarification.summary.clone(),
        due: clarification.due.clone(),
        priority: clarification.priority,
        parent,
        ..Default::default()
    };

    let mut contents = fields.apply(&item.contents)?;

    contents = match outcome {
        ClarifyOutcome::Done => complete(&contents, now)?,
        ClarifyOutcome::Someday => edit_categories(
            &contents,
            Some(GtdState::Someday),
            &clarification.categories,
        )?,
        ClarifyOutcome::Waiting => {
            let contents = edit_categories(
                &contents,
                Some(GtdState::Waiting),
                &clarification.categories,
            )?;
            set_contact(&contents, clarification.contact.as_deref())?
        }
        ClarifyOutcome::Next => edit_categories(&contents, None, &clarification.categories)?,
        ClarifyOutcome::Delete => unreachable!("handled above"),
    };

    if outcome == ClarifyOutcome::Done {
        // NOTE: a done task leaves every view whatever it carries, yet it
        // is clarified, so the capture mark goes too.
        contents = edit_categories(&contents, None, &clarification.categories)?;
    }

    let contents = revise(&contents, now)?;

    let Some((account, tasklist)) = target else {
        scope.client(found.account).update_item(
            &found.tasklist,
            &found.task.id,
            contents,
            etag.as_deref(),
        )?;
        return Ok(clarified);
    };

    // NOTE: within one account a backend that can move in one write does
    // (pimdir: an add superseding the source, whose removal waits for
    // the add to be pushed; CalDAV: a `MOVE`).
    let moved = match account == found.account {
        true => scope
            .client(account)
            .move_item(
                &found.tasklist,
                &found.task.id,
                &tasklist,
                contents.clone(),
                etag.as_deref(),
            )
            .with_context(|| {
                format!("Move the task to tasklist `{tasklist}`; it was left where it was")
            })?,
        false => None,
    };

    if let Some(id) = moved {
        clarified.tasklist = tasklist;
        clarified.id = id;
        clarified.moved = true;
        return Ok(clarified);
    }

    // NOTE: created in the target first, removed from the source only
    // once that create was accepted, so a refused create keeps the
    // capture where it was.
    let id = scope
        .client(account)
        .create_item(&tasklist, contents)
        .with_context(|| {
            format!(
                "Move the task to tasklist `{tasklist}` of account `{}`; it was left where it \
                 was",
                scope.name(account)
            )
        })?;

    scope
        .client(found.account)
        .delete_item(&found.tasklist, &found.task.id, etag.as_deref())
        .with_context(|| {
            format!(
                "The task was copied to tasklist `{tasklist}` as `{id}`, but removing it from \
                 `{}` failed; remove it there by hand",
                found.tasklist
            )
        })?;

    clarified.account = scope.name(account).to_owned();
    clarified.tasklist = tasklist;
    clarified.id = id;
    clarified.moved = true;

    Ok(clarified)
}

/// One clarified task.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ClarifiedTask {
    /// The account the task lives in now.
    pub account: String,
    /// The tasklist the task lives in now.
    pub tasklist: String,
    /// Its handle there.
    pub id: String,
    /// Its `UID`.
    pub uid: String,
    /// What it is.
    pub outcome: ClarifyOutcome,
    /// Whether it moved to another tasklist.
    pub moved: bool,
    /// Whether it was in that state already, and left untouched.
    #[serde(skip_serializing_if = "core::ops::Not::not")]
    pub unchanged: bool,
}

/// What `clarify` did.
#[derive(Clone, Debug, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ClarifyOutput {
    /// The tasks clarified, in order.
    pub clarified: Vec<ClarifiedTask>,
}

impl fmt::Display for ClarifyOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for task in &self.clarified {
            let what = match task.outcome {
                ClarifyOutcome::Delete => "deleted",
                ClarifyOutcome::Someday => "kept for Someday",
                ClarifyOutcome::Done => "marked as done",
                ClarifyOutcome::Waiting => "waiting",
                ClarifyOutcome::Next => "a next action",
            };

            match (task.unchanged, task.moved) {
                (true, _) => writeln!(f, "Task `{}` is {what} already, left untouched", task.uid)?,
                (_, true) => writeln!(
                    f,
                    "Task `{}` {what}, moved to tasklist `{}` of account `{}`",
                    task.uid, task.tasklist, task.account
                )?,
                _ => writeln!(f, "Task `{}` {what}", task.uid)?,
            }
        }

        if self.clarified.is_empty() {
            writeln!(f, "Nothing clarified")?;
        }

        Ok(())
    }
}
