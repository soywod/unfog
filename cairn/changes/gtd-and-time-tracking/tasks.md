---
cairn: tasks
change: gtd-and-time-tracking
---

# Tasks

Waits on pimdir and io-pimdir cairn/changes/tasks-by-categories for the pimdir backend's views and local time collection; the CalDAV and vdir halves do not.

## Phase 0
- [x] Open questions settled: `local` by default, top-level verbs (2026-10-28)
- [x] delta.md: a gtd capability and a time capability under cairn/spec, rust-rewrite's Out and "What 1.x users lose" modified

## Phase 1: GTD
- [x] The convention in src/shared/task: state from `CATEGORIES`, case-insensitive; writes touch only the three values, other categories kept in order
- [x] `capture` (text or stdin, the capture collection)
- [x] `inbox`, `next [--category]`, `waiting`, `someday` across accounts; pimdir through the guide's queries
- [x] `clarify`: interactive and by flags; edits with `If-Match`; a collection change is create then remove once accepted
- [x] `review`
- [x] `--json` and JSON Schema for each verb
- [x] Tests: each view on vdir and pimdir fixtures, a cased `Inbox`, a person's category kept, a refused create keeps the capture

## Phase 2: time tracking
- [x] `start`, `stop`, `status`, `worktime`
- [x] Strategies `event`, `journal`, `local` (pimdir `U+0001time`, else a vdir directory)
- [x] `time migrate --to <strategy>`
- [x] Tests: each strategy round trips; a start stops the running entry; `worktime` sums across strategies

## Phase 3: import and landing
- [x] Import: projects as categories, start and stop into the configured strategy
- [x] README: unfog as a GTD manager, the strategies
- [x] Fold, log, CHANGELOG, status landed

## Notes

Answers taken during the session of 2026-10-28, to review.

- io-pimdir is patched to the sibling checkout (`path = "../io-pimdir"`) for `put_item`, `ensure_time_collection` and `TIME_COLLECTION`, until that change is pushed and pinned to a rev.
- pimdir views read `task_summary.status` and `categories` rather than the FTS index: io-pimdir's `search` feature does not build against the pinned ical-rs (`caa1422`, it needs `99b9273`'s `tzdb::version`), a first refresh would index the whole store, mail included, on a first `unfog inbox`, and the index does not fold the pending queue, so a staged clarification would not show. The views' `--json` carries the guide's query string.
- The scope is every configured account (the default first, then by name), each through the backend `--backend` selects; `-a` narrows it to one. Short ids in a view are counted over the view; a prefix too short for the whole scope is refused as ambiguous, never guessed.
- `capture` falls back to `tasklist.default` where the app takes the last used tasklist: unfog keeps no state. Stdin's first line is the summary, the rest the description.
- `clarify --done` also removes the reserved values; `--to-account` moves across accounts; `--contact` is checked at run time (clap counts an unset bool flag of the outcome group as present, so `requires` cannot say it).
- The interactive `clarify` is untested here (it needs a terminal); every outcome it reaches goes through the tested `apply`.
- Time entries are written in UTC. An event entry's `RELATED-TO` names no `RELTYPE` (the default `PARENT`). A journal entry's `UID` is the start's; journal marks pair per task in time order within one place.
- `start` refuses a task already running; `stop` with nothing running prints "Nothing runs" and succeeds.
- On pimdir, `event` and `journal` entries are staged `add`s, listed once a sync applies them, so `stop` before a sync finds no running event; `local` is written by the store's owner at once, and fails at once while a sync holds the owner lock. Opening the store as its owner also reconciles its schema to this build's io-pimdir, so unfog and the sync engine should share one io-pimdir version.
- `worktime` reports per task, per category and in total, not per day as 1.x's `--more` did.
- `import --journal` is gone; time entries are keyed `<task UID>-<start Unix seconds>` so a second import skips them.
- No CalDAV mock exists in the crate: the CalDAV paths of the views and of the `event` and `journal` strategies go through the same `TaskClient` calls the vdir tests cover, untested against a server.
