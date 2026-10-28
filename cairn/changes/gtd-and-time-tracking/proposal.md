---
cairn: change
id: gtd-and-time-tracking
status: landed
created: 2026-10-28
---

# unfog, a GTD task manager with pluggable time tracking

## Why

rust-rewrite (2026-10-27) kept unfog to VTODO and left GTD and time tracking to layers on top. That leaves the terminal without the method Pimalaya Android ships (pimalaya/android cairn/changes/gtd-by-categories), and 1.x users without `start`, `stop` and `worktime`. The GTD convention is now light enough to live in unfog itself: three reserved categories over plain VTODOs (pimalaya/pimdir cairn/changes/tasks-by-categories), so unfog and the app read and write the same tasks the same way, and a task captured on the phone is clarified in the terminal.

Time tracking has no standard. Rather than pick one encoding, unfog offers the shapes pimdir's guide describes and lets the person choose where time goes.

This change modifies rust-rewrite's split: unfog stays the VTODO CLI for editing, and writes VEVENT or VJOURNAL only as time entries, nothing else. calendula and the journal CLI keep their domains.

## What

### GTD

The convention, exactly as the app applies it: `inbox`, `waiting` and `someday` as `CATEGORIES` values, compared case-insensitively, stored as those literals; a next action is an open task with none of them, open being any status but `COMPLETED` and `CANCELLED`, a task stating no `STATUS` included; every other category is the person's (contexts, projects).

- `unfog capture <text>`: a VTODO with `CATEGORIES:inbox` in the capture collection (`-l`, else `capture.tasklist`, else `tasklist.default`, else the backend's default task collection, else the only one; unfog keeps no state, so the app's "last used" is `tasklist.default`). Stdin when no text, so a script or a mail filter can capture.
- `unfog inbox`, `unfog next [--category <c>]...`, `unfog waiting`, `unfog someday`: the four views, across every configured account and collection. On pimdir they are the guide's queries evaluated over the store's task summaries (`task_summary.status` and `categories`, the columns the queries read), a staged edit read from its body; on CalDAV and vdir the same predicates over the listed tasks. The FTS index (io-pimdir's `search` feature) is not used: it needs a newer ical-rs pin, a first refresh indexes the whole store, mail included, and it does not fold the pending queue.
- `unfog clarify [<id>]`: the guided flow over the Inbox, one item at a time, the same steps as the app (rephrase, delete, someday, done, waiting with an optional `--contact`, next with categories and a due date, optionally another collection). Each step also exists as flags for scripts: `unfog clarify <id> --summary .. --waiting|--someday|--next --category ..`. A result is an edit of the same item with `If-Match`; a collection change creates in the target and removes from the source once the create is accepted.
- `unfog review`: the weekly review as a sequence of the views, counts first.
- `task list --category` and `task update --category` stay the raw access, so nothing in the convention is hidden.

### Time tracking

`unfog start <id>`, `unfog stop [<id>]`, `unfog status` (what runs, since when), `unfog worktime [--since] [--until] [--category <c>]` (durations per task and per category, 1.x's report). One strategy is configured per account, `time.strategy`:

| Strategy | Entry | Where | Synced |
| --- | --- | --- | --- |
| `event` | a VEVENT: `DTSTART`, `DTEND` on stop, `TRANSP:TRANSPARENT`, `RELATED-TO` the task's `UID` | a calendar collection the person names | yes, any calendar server |
| `journal` | a VJOURNAL pair, `CATEGORIES:start` and `stop`, `RELATED-TO` the task | a collection taking VJOURNAL | yes, where the server takes VJOURNAL |
| `local` (default) | the `event` shape | pimdir's local time collection (`U+0001time`) on the pimdir backend, a vdir directory on the others | no, like 1.x's store |

`local` is the default because it needs no server and matches 1.x; switching to `event` later is `unfog time migrate --to event`, a copy of the same bytes then a removal (a journal pair and an event are converted into each other). `event` and `journal` write to the tasklist `time.collection` names; `local` writes to `time.directory` (`$XDG_DATA_HOME/unfog/time` by default) outside pimdir. `worktime` reads every strategy configured, so history survives a switch. A start while another entry runs stops it first (one running entry per person, as 1.x).

### Import of the Haskell store

As rust-rewrite states, except: the 1.x project becomes a plain category, and `TaskStarted`/`TaskStopped` pairs become entries of the configured strategy (`local` by default) instead of a VJOURNAL file for another CLI.

### What 1.x users keep

`start`, `stop`, `status` and `worktime` stay. `context` is replaced by `next --category`.

## Out

- Alarms on time entries, reports beyond `worktime`'s table (the `--json` output is for those).
- Google Tasks and Microsoft To Do GTD: they follow rust-rewrite's backends (M3, M4); To Do carries `categories`, Google Tasks has none, so GTD there is an open question of those milestones.

## Open questions

- The default strategy: `local` (no server, 1.x parity, single device) or `event` (synced, visible in every calendar). Recommended `local`, with the README pointing at `event` for more than one device.
- Whether `capture`, `inbox`, `next` and the rest are top-level verbs or a `gtd` family (`unfog gtd next`). Recommended top-level: they are what unfog is for.
