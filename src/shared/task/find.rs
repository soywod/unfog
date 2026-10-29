//! # Task ids
//!
//! How a task is named on the command line: by the handle of the item
//! holding it, or by a prefix of its `UID` as unfog 1.x addressed tasks,
//! dashes ignored on both sides. The lookup itself spans the scope
//! ([`crate::scope::Scope::find`]); this module holds the short ids a
//! listing prints.

/// The shortest prefix a listing shows, as 1.x did.
const SHORT_MIN: usize = 4;

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
    use crate::shared::task::find::{short_id, short_len};

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
