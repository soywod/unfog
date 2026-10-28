---
cairn: delta
change: gtd-and-time-tracking
---

## ADDED Requirements

The requirements below fold into two new capabilities: spec/gtd.md (the convention, the views, capture, clarify, review) and spec/time.md (time entries, strategies, the tracking verbs, the migration).

### Requirement: GTD state is three reserved categories

A task's GTD state SHALL be read from its `CATEGORIES`: `inbox`, `waiting` and `someday` are reserved, compared after the Unicode default lowercase mapping and a trim (pimdir STORAGE Annex A.2's rule for `fn`), and written as those English literals. Every other category SHALL be the person's own. unfog SHALL NOT read or write an X- property for GTD.

A task SHALL be open when its `STATUS` is anything but `COMPLETED` and `CANCELLED`, a task stating no `STATUS` included.

#### Scenario: A cased Inbox
- GIVEN an open task carrying `CATEGORIES:Inbox`
- WHEN `unfog inbox` runs
- THEN the task is listed, and `unfog next` does not list it

### Requirement: The views

`inbox`, `waiting` and `someday` SHALL list the open tasks carrying that reserved value; `next` SHALL list the open tasks carrying none of the three. `next --category <TEXT>` (repeated) SHALL keep the tasks carrying every category named, compared as the reserved values are. Each view SHALL be the predicate of pimdir GUIDE.md §17's query, which its `--json` output carries as `query`:

| View | Query |
| --- | --- |
| Inbox | `kind:task -status:completed -status:cancelled category:inbox` |
| Waiting | `kind:task -status:completed -status:cancelled category:waiting` |
| Someday | `kind:task -status:completed -status:cancelled category:someday` |
| Next | `kind:task -status:completed -status:cancelled -category:inbox -category:waiting -category:someday` |

Rows SHALL be sorted as `task list` sorts them, and carry the account, the tasklist and a short id counted over the view.

#### Scenario: A to-do another client wrote is a next action
- GIVEN a task carrying no `STATUS` and `CATEGORIES:home`
- WHEN `unfog next` runs
- THEN the task is listed

#### Scenario: Closed tasks are in no view
- GIVEN a completed task carrying `inbox` and a cancelled task carrying nothing
- WHEN each view runs
- THEN neither is listed

### Requirement: The views span every account

A view, `clarify`, `review` and the time commands SHALL span every configured account, each through the backend `--backend` selects for it, and every tasklist of each; `-a/--account` SHALL narrow them to that account. An account carrying no backend `--backend` allows SHALL be left out unless named. A task SHALL be found across that scope by its handle or a `UID` prefix, a prefix naming several tasks refused naming them.

### Requirement: The views on each backend

On CalDAV and vdir a view SHALL list each tasklist's VTODOs and apply its predicate. On pimdir it SHALL apply the predicate to the store's task summaries (`task_summary.status` and `categories`, the columns the guide's queries read), reading a body only for a task it keeps, and deciding from the body a task a pending `update` repoints, so a staged clarification shows at once.

#### Scenario: A staged clarification on pimdir
- GIVEN a pimdir store holding a task carrying `inbox`
- WHEN `unfog clarify <TASK> --next` stages its update, and no sync ran
- THEN `unfog inbox` no longer lists it and `unfog next` does

### Requirement: Capture

`unfog capture <TEXT>...` SHALL write a VTODO carrying a fresh `UID`, `DTSTAMP`, the text as `SUMMARY` and `CATEGORIES:inbox`, to the tasklist `-l` names, else `capture.tasklist`, else `tasklist.default`, else the tasklist the backend marks as the default, else the only one, of the account `-a` names or the default one. With no text it SHALL read stdin, its first line the summary and the rest the description, and refuse a terminal.

### Requirement: Clarify

`unfog clarify <TASK>` with one outcome flag (`--delete`, `--someday`, `--done`, `--waiting`, `--next`) SHALL clarify that task; `--summary` SHALL rephrase it, `--category` (repeated) add categories, `--due` set its due date, `--contact` (with `--waiting` only) replace its `CONTACT`s, `--to <TASKLIST>` and `--to-account <ACCOUNT>` move it. With no outcome flag it SHALL walk the Inbox (or the task named) interactively, asking the app's steps: rephrase, actionable or not (Someday, delete, skip), done in under two minutes, someone else's (with whom), else a next action with categories, a due date and a tasklist; the interactive flow SHALL refuse `--json` and a non-terminal stdin.

`--someday` and `--waiting` SHALL replace the reserved values by theirs, `--next` and `--done` SHALL remove them, `--done` also completing the task as `task done` does, and `--delete` SHALL delete it. A writer SHALL edit only the three reserved values: the state lands where the first reserved value stood, else on a new `CATEGORIES` line after the last, which also takes the added categories; every other category SHALL keep its place, a `CATEGORIES` line holding no reserved value its bytes, and every other line its bytes but `DTSTAMP`. Each edit SHALL be gated on the version read (`If-Match`).

A move SHALL create the task in the target and remove it from the source only once the create was accepted. A deleted or done task SHALL NOT be moved.

#### Scenario: A person's category is kept
- GIVEN a task carrying `CATEGORIES:work,Inbox,q3` and `CATEGORIES:phone-ish`
- WHEN `unfog clarify <TASK> --next -C phone` runs
- THEN the task carries `CATEGORIES:work,q3`, `CATEGORIES:phone-ish` and `CATEGORIES:phone`, and every other line but `DTSTAMP` as it was

#### Scenario: A refused create keeps the capture
- GIVEN a task carrying `inbox`
- WHEN `unfog clarify <TASK> --next --to <TASKLIST>` runs and the create in the target is refused
- THEN the command fails, the task is still in the Inbox where it was, and nothing is staged

### Requirement: Review

`unfog review` SHALL print how many tasks Inbox, Next, Waiting and Someday hold, then each view in that order.

### Requirement: The GTD and time verbs are top-level

`capture`, `inbox`, `next`, `waiting`, `someday`, `clarify`, `review`, `start`, `stop`, `status`, `worktime` and `time` SHALL be top-level commands, ahead of the shared API. Each SHALL hand the printer a named `*Output` type registered in the json-schema registry under `unfog-<verb>` (`unfog-time-migrate` for `time migrate`), with an object root.

### Requirement: Time entries

A time entry SHALL be tied to its task by `RELATED-TO` the task's `UID` (the default `PARENT` relation), in one of two shapes (pimdir GUIDE.md §18):

- an event: a VEVENT with `UID`, `DTSTAMP`, `DTSTART`, `DTEND` once stopped, the task's `SUMMARY` and `TRANSP:TRANSPARENT`; a running one carries no `DTEND`;
- a journal: a pair of VJOURNALs, `DTSTART` the instant and `CATEGORIES:start` or `CATEGORIES:stop`, each its own item.

When reading, a VEVENT SHALL be an entry when it carries a `RELATED-TO`, and a VJOURNAL half of one when it also carries `start` or `stop` (any case); the marks of a task SHALL pair in time order, a start left alone running and a stop with no start ignored. Times SHALL be written in UTC and read in their `TZID` zone, else the local one.

### Requirement: Time strategies

`time.strategy` SHALL say where `start` writes, per account (or globally): `event` and `journal` in the tasklist `time.collection` names (refused when unset), `local` (the default) in the event shape on the device: the local time collection of the pimdir store (`U+0001time`, declared with `ensure_time_collection`, written as the store's owner with `put_item`, removed with `delete_unbound`, never queued) on the pimdir backend, else the vdir directory `time.directory` names, `$XDG_DATA_HOME/unfog/time` by default. The pimdir backend SHALL NOT list the local time collection as a tasklist.

#### Scenario: Each strategy round trips
- GIVEN an account of each strategy
- WHEN `unfog start <TASK>` runs at 09:00 and `unfog stop` at 10:30
- THEN `unfog worktime` reports 1h30 for the task, the entry an event with `DTEND` in the local place or in `time.collection`, or a `start` and a `stop` VJOURNAL in `time.collection`

### Requirement: One running entry

`unfog start <TASK>` SHALL stop every running entry of every account first, and refuse a task already running. `unfog stop` SHALL stop every running entry, `unfog stop <TASK>` that task's alone and fail when it does not run; an event SHALL be stopped by its `DTEND` (and `DTSTAMP`), every other line kept, a journal by a `stop` VJOURNAL in the place holding its start. `unfog status` SHALL list the running entries and since when.

#### Scenario: A start stops the running entry
- GIVEN an entry running for task A since 09:00, in another account
- WHEN `unfog start B` runs at 10:00
- THEN A's entry ends at 10:00 and only B's runs

### Requirement: Worktime

`unfog worktime` SHALL sum every entry of every place each account keeps entries in (the local place and `time.collection`), whatever its strategy now, each entry once by `UID`, a running one up to now; `--since` and `--until` (a day at local midnight or an RFC 3339 instant) SHALL cut entries to that span, and `--category` (repeated) keep the tasks carrying every category named. It SHALL report the time per task, per category (the reserved values aside, a task counting in each of its categories) and in total.

#### Scenario: Worktime sums across strategies
- GIVEN 1h kept under `local`, then 30m under `event`, then 15m under `journal`
- WHEN `unfog worktime` runs under `journal`
- THEN it reports 1h45 in total

### Requirement: Time migration

`unfog time migrate --to <STRATEGY>` SHALL copy every entry an account keeps anywhere but where that strategy writes, in its shape, to that place, and remove it from where it was only once the copy was accepted: the same bytes when the shape is kept, the other shape's spelling of the same span otherwise. It SHALL NOT change the configuration.

## MODIFIED Requirements

These modify requirements of cairn/changes/rust-rewrite, still active and not folded: they are applied to that change's delta.md, which folds them with the rest.

### Requirement: One component kind, VTODO

unfog SHALL read and write VTODO components for tasks. A task listing SHALL keep the VTODOs of a collection and drop every other component kind. unfog SHALL write a VEVENT or a VJOURNAL only as a time entry (spec/time.md), never otherwise.

#### Scenario: A mixed collection
- GIVEN a collection holding one VTODO and one VEVENT
- WHEN `task list` runs on it
- THEN one task is listed, the VTODO

### Requirement: The command tree

The command tree SHALL be split into the GTD verbs and time tracking (`capture`, `inbox`, `next`, `waiting`, `someday`, `clarify`, `review`, `start`, `stop`, `status`, `worktime`, `time`), the shared API (`tasklist`, `task`, `import`), the backend families (`caldav`, `vdir`, `pimdir`, each behind its own cargo feature), and the meta commands (`configure`, `account`, `completion`, `manual`, `json-schema`), in that order. Commands SHALL be singular, each plural a hidden alias. The shared API SHALL expose only operations every compiled backend serves identically; a concept one backend lacks SHALL live in that backend's family.

1.x's `context` SHALL have no command of its own: `next --category` replaces it.

### Requirement: The time entries of an import

`TaskStarted` and each `TaskStopped` closing it SHALL become one time entry of the account's `time.strategy`, in its place and shape: an event `DTSTART` the start and `DTEND` the stop (none when the store ends with the task running), or a `start` and a `stop` VJOURNAL. A `TaskDid` or `TaskDeleted` closing a started task SHALL add the stop 1.x implied. An entry's `UID` SHALL be the task's `UID` and the start in Unix seconds (`-start` and `-stop` suffixed for a journal pair, the stop's instant in the second), and an entry whose start item the place already holds SHALL be skipped and counted, so an import run twice writes nothing the second time. `--journal` is gone.

#### Scenario: A worked task
- GIVEN a store where a task was started at 09:00Z and stopped at 10:00Z
- WHEN it is imported under the default strategy
- THEN the local place holds one VEVENT from 09:00Z to 10:00Z related to the task's `UID`, and a second import adds none

### Requirement: Import of the 1.x store

Unchanged but for one reading: the last project SHALL become one plain `CATEGORIES` value and no reserved one is added, so an imported open task is a next action (a project spelled `inbox`, `waiting` or `someday` reads as that state).

### Requirement: Configuration

As before, plus: `capture.tasklist`, and `time.strategy` (`event`, `journal`, `local`), `time.collection` and `time.directory`, each settable globally and per account.

## REMOVED Requirements

None.
