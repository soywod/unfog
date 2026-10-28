//! # Task lookup
//!
//! How a task named on the command line is found: by the handle of the
//! item holding it, or by a prefix of its `UID` as unfog 1.x addressed
//! tasks, dashes ignored on both sides.

use anyhow::{Result, bail};
use ical::component::IcalComponentKind;

use crate::shared::{
    client::TaskClient,
    item::{TaskItem, TaskItemQuery},
    task::Task,
};

/// The shortest prefix a listing shows, as 1.x did.
const SHORT_MIN: usize = 4;

/// Finds the item holding the task `query` names in `tasklist_id`, read
/// afresh so its entity tag guards the write that follows.
///
/// A handle wins over a prefix. A prefix matching no task is refused as
/// not found, and one matching several tasks is refused naming them,
/// never guessed.
pub fn find_task(client: &mut TaskClient, tasklist_id: &str, query: &str) -> Result<TaskItem> {
    let query = query.trim();

    if query.is_empty() {
        bail!("Cannot find a task from an empty id");
    }

    let listing = TaskItemQuery {
        kind: Some(IcalComponentKind::VTodo),
        ..Default::default()
    };

    let items = client.list_items(tasklist_id, listing)?;
    let tasks: Vec<Task> = items.iter().flat_map(Task::project).collect();

    let id = match matching(&tasks, query)? {
        Some(id) => id,
        None => bail!("Cannot find task `{query}` in tasklist `{tasklist_id}`"),
    };

    client.get_item(tasklist_id, &id)
}

/// The handle of the one task `query` names among `tasks`, `None` when
/// it names none.
fn matching(tasks: &[Task], query: &str) -> Result<Option<String>> {
    if let Some(task) = tasks.iter().find(|task| task.id == query) {
        return Ok(Some(task.id.clone()));
    }

    let prefix = undashed(query);

    let mut found: Vec<&Task> = tasks
        .iter()
        .filter(|task| undashed(&task.uid).starts_with(&prefix))
        .collect();

    // NOTE: an item may hold several VTODOs sharing one UID (a recurring
    // task and its overrides), which are one task to address.
    found.dedup_by(|a, b| a.id == b.id);

    match found.as_slice() {
        [] => Ok(None),
        [task] => Ok(Some(task.id.clone())),
        several => {
            let uids: Vec<&str> = several.iter().map(|task| task.uid.as_str()).collect();
            bail!(
                "Task id `{query}` is ambiguous, it prefixes {}; give more of it",
                uids.join(", ")
            )
        }
    }
}

/// The length of the short ids a listing of `uids` shows: the shortest,
/// from 4 up, under which no two dash-less `UID`s share a prefix.
pub fn short_len<'a>(uids: impl IntoIterator<Item = &'a str>) -> usize {
    let mut uids: Vec<String> = uids.into_iter().map(undashed).collect();
    uids.sort();
    uids.dedup();

    let longest = uids.iter().map(String::len).max().unwrap_or(0);

    (SHORT_MIN..longest.max(SHORT_MIN))
        .find(|len| {
            uids.windows(2)
                .all(|pair| short(&pair[0], *len) != short(&pair[1], *len))
        })
        .unwrap_or(longest.max(SHORT_MIN))
}

/// The short id of `uid` at `len`.
pub fn short_id(uid: &str, len: usize) -> String {
    short(&undashed(uid), len).to_owned()
}

/// The first `len` characters of a dash-less id.
fn short(uid: &str, len: usize) -> &str {
    match uid.char_indices().nth(len) {
        Some((at, _)) => &uid[..at],
        None => uid,
    }
}

/// An id with its dashes removed, which is how 1.x compared them.
pub(crate) fn undashed(id: &str) -> String {
    id.chars().filter(|c| *c != '-').collect()
}

#[cfg(test)]
mod tests {
    use crate::shared::task::{
        Task,
        find::{matching, short_id, short_len},
    };

    fn task(id: &str, uid: &str) -> Task {
        Task {
            id: id.into(),
            uid: uid.into(),
            ..Default::default()
        }
    }

    fn tasks() -> Vec<Task> {
        vec![
            task("a.ics", "4c1e0b2a-7d3e-4f00-9b1a-000000000001"),
            task("b.ics", "4c1e77d0-0000-4000-8000-000000000002"),
            task("c.ics", "9f00aa11-0000-4000-8000-000000000003"),
        ]
    }

    #[test]
    fn a_prefix_finds_its_task_dashes_ignored() {
        assert_eq!(
            matching(&tasks(), "9f00").unwrap().as_deref(),
            Some("c.ics")
        );
        assert_eq!(
            matching(&tasks(), "4c1e0b2a7d").unwrap().as_deref(),
            Some("a.ics")
        );
        assert_eq!(
            matching(&tasks(), "4c1e-0b2a").unwrap().as_deref(),
            Some("a.ics")
        );
    }

    #[test]
    fn a_handle_wins_over_a_prefix() {
        let mut tasks = tasks();
        tasks.push(task("9f00", "1111"));

        assert_eq!(matching(&tasks, "9f00").unwrap().as_deref(), Some("9f00"));
    }

    #[test]
    fn an_ambiguous_prefix_is_refused_naming_every_match() {
        let err = matching(&tasks(), "4c1e").unwrap_err().to_string();

        assert!(err.contains("4c1e0b2a-7d3e"), "{err}");
        assert!(err.contains("4c1e77d0-0000"), "{err}");
    }

    #[test]
    fn an_unknown_prefix_finds_nothing() {
        assert_eq!(matching(&tasks(), "zz").unwrap(), None);
    }

    #[test]
    fn short_ids_grow_until_every_task_is_told_apart() {
        let uids = ["4c1e0b2a", "4c1e77d0", "9f00aa11"];
        let len = short_len(uids);

        assert_eq!(len, 5);
        assert_eq!(short_id("4c1e-0b2a", len), "4c1e0");

        assert_eq!(short_len(["abcdef"]), 4);
        assert_eq!(short_len([]), 4);
    }
}
