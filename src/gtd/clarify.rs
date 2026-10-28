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
        gtd::{GtdState, GtdView, edit_categories, set_contact},
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

        let mut clarified = Vec::new();
        let total = queue.len();

        for (index, found) in queue.iter().enumerate() {
            eprintln!();
            eprintln!("({}/{total}) {}", index + 1, found.task.summary);

            let Some(clarification) = ask(&mut scope, found)? else {
                continue;
            };

            clarified.push(apply(&mut scope, found, &clarification, now)?);
        }

        printer.out(ClarifyOutput { clarified })
    }
}

/// Asks what a task is, `None` when it is skipped.
fn ask(scope: &mut Scope, found: &Located) -> Result<Option<Clarification>> {
    let summary = prompt::text("Rephrase it:", Some(found.task.summary.as_str()))?;
    let summary = (summary.trim() != found.task.summary).then_some(summary);

    let mut clarification = Clarification {
        summary,
        outcome: ClarifyOutcome::Next,
        contact: None,
        categories: Vec::new(),
        due: None,
        priority: None,
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

    let mut targets: Vec<(usize, String)> = Vec::new();
    for index in 0..scope.accounts.len() {
        for tasklist in scope.client(index).list_tasklists()? {
            targets.push((index, tasklist.id));
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
    };

    if outcome == ClarifyOutcome::Delete {
        scope.client(found.account).delete_item(
            &found.tasklist,
            &found.task.id,
            etag.as_deref(),
        )?;
        return Ok(clarified);
    }

    let fields = TaskFieldsArgs {
        summary: clarification.summary.clone(),
        due: clarification.due.clone(),
        priority: clarification.priority,
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

            match task.moved {
                true => writeln!(
                    f,
                    "Task `{}` {what}, moved to tasklist `{}` of account `{}`",
                    task.uid, task.tasklist, task.account
                )?,
                false => writeln!(f, "Task `{}` {what}", task.uid)?,
            }
        }

        if self.clarified.is_empty() {
            writeln!(f, "Nothing clarified")?;
        }

        Ok(())
    }
}
