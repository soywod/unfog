---
cairn: delta
change: gtd-hardening
---

## MODIFIED Requirements

The requirements below replace their namesakes in spec/time.md (time entries, strategies, one running entry, worktime, migration) and spec/gtd.md (the views, their scope and backends, capture, clarify, review). Draft, written before the code of tracks C and D; refined as they land.

### Requirement: Time entries

A time entry SHALL be tied to its task by `RELATED-TO` the task's `UID` (the default `PARENT` relation), in one of two shapes (pimdir GUIDE.md §18):

- an event: a VEVENT with `UID`, `DTSTAMP`, `DTSTART`, `DTEND` once stopped, the task's `SUMMARY` and `TRANSP:TRANSPARENT`; a running one carries no `DTEND`;
- a journal: a pair of VJOURNALs, `DTSTART` the instant and `CATEGORIES:start` or `CATEGORIES:stop`, each its own item.

When reading, a VEVENT SHALL be an entry when it carries `TRANSP:TRANSPARENT` and a `RELATED-TO` naming a task of the scope, and a VJOURNAL half of one when it carries such a `RELATED-TO` and `start` or `stop` (any case); every other component SHALL be left alone, never rewritten. A VEVENT's `DURATION` SHALL be its end as a `DTEND` is, so an event carrying either is stopped. The marks of a task SHALL pair in time order, a start left alone running and a stop with no start ignored. Times SHALL be written in UTC and read in their `TZID` zone, else the local one.

`time.collection` SHOULD name a calendar of its own, holding nothing but time entries.

#### Scenario: A meeting is not an entry
- GIVEN `time.collection` holding a one-hour meeting, `DTSTART`, `DURATION:PT1H` and `RELATED-TO:agenda-doc-42`, opaque
- WHEN `unfog task status` then `unfog task start <TASK>` run
- THEN nothing runs, and the meeting's bytes are as they were

### Requirement: Time strategies

`time.strategy` SHALL say where a finished entry is written, per account (or globally): `event` and `journal` in the calendar `time.collection` names (refused when unset, the message naming a calendar that accepts VEVENT, or VJOURNAL for `journal`), of the account `time.account` names, else of the task's own; `local` (the default) in the event shape on the device: the local time collection of the pimdir store (`U+0001time`, written as the store's owner, never queued) on the pimdir backend, else the vdir directory `time.directory` names, `$XDG_DATA_HOME/unfog/time` by default. The pimdir backend SHALL NOT list the local time collection as a tasklist.

A running entry SHALL always be kept in the local place, whatever the strategy: `task start` writes it there and `task stop` writes the finished entry where the strategy says, then removes the running one, so no running entry ever waits in a queue or on a server.

unfog SHALL NOT rebuild a pimdir store: a write to the local time collection SHALL open the store as its owner only when it is at this build's schema, refusing an absent store or one whose schema differs (pimdir STORAGE §6) and naming its sync as the way out, and SHALL retry for a short bounded time (about two seconds) while a sync holds the owner lock, then fail naming it. A collection on the pimdir backend SHALL list its pending adds, so a stopped entry counts before a sync.

#### Scenario: Each strategy round trips
- GIVEN an account of each strategy
- WHEN `unfog task start <TASK>` runs at 09:00 and `unfog task stop` at 10:30
- THEN `unfog worktime` reports 1h30 for the task, the entry an event with `DTEND` in the local place or in `time.collection`, or a `start` and a `stop` VJOURNAL in `time.collection`

#### Scenario: A start on pimdir is seen before any sync
- GIVEN an account on the pimdir backend under `event`
- WHEN `unfog task start A` runs, then `unfog task status`, then `unfog task start B`, then `unfog task stop`, with no sync between
- THEN status lists A, the second start stops A first, the stop stops B, and the queue holds two adds, each a finished event

### Requirement: One running entry

`unfog task start <TASK>` SHALL stop every running entry of every account first, and refuse a task already running. `unfog task stop` SHALL stop every running entry, `unfog task stop <TASK>` that task's alone and fail when it does not run. A running entry SHALL be stopped by writing it finished where its account's strategy says (an event with `DTEND` and `DTSTAMP`, every other line kept, or a `start` and `stop` VJOURNAL pair), as one write per item, then removing it from the local place; a running entry found elsewhere (written by an older unfog) SHALL be stopped where it is. `unfog task status` SHALL list the running entries and since when.

#### Scenario: A start stops the running entry
- GIVEN an entry running for task A since 09:00, in another account
- WHEN `unfog task start B` runs at 10:00
- THEN A's entry ends at 10:00 and only B's runs

### Requirement: Worktime

`unfog worktime` SHALL sum every entry of every place each account keeps entries in (the local place and `time.collection`), whatever its strategy now, each entry once by `UID`, a running one up to now; `--since` and `--until` (a day at local midnight or an RFC 3339 instant) SHALL cut entries to that span, and `--category` (repeated) keep the tasks carrying every category named. It SHALL report the time per task, per category (the reserved values aside, a task counting in each of its categories, categories compared after the Unicode default lowercase mapping and a trim, the first spelling met shown) and in total.

#### Scenario: Two spellings are one category
- GIVEN 1h on a task carrying `Work` and 30m on one carrying `work`
- WHEN `unfog worktime` runs
- THEN it reports one category row of 1h30

### Requirement: Time migration

`unfog migrate --to <STRATEGY>` SHALL copy every finished entry an account keeps anywhere but where that strategy writes, in its shape, to that place, and remove it from where it was only once the copy was accepted: the same bytes when the shape is kept, the other shape's spelling of the same span otherwise, keeping the entry's `UID` (a journal pair's stop takes `<UID>-stop`; a conversion within one calendar, where a server holds one item per `UID`, takes `<UID>-event` or `<UID>-journal`). An entry whose `UID` is already at the target SHALL NOT be copied again, only removed from the source, so a migration cut short resumes. On the pimdir backend a copy is a queued add, which the collection lists at once (see the views on each backend), so the source copy is removed as soon as the add is staged. It SHALL NOT change the configuration, and SHALL leave running entries in the local place.

#### Scenario: A migration resumes
- GIVEN a migration to `event` that copied an entry and failed before removing it from the local place
- WHEN `unfog migrate --to event` runs again
- THEN the target holds one copy and the local place none, and `unfog worktime` counts it once

### Requirement: The views

`gtd inbox`, `gtd waiting` and `gtd someday` SHALL list the open tasks carrying that reserved value; `gtd next` SHALL list the open tasks carrying none of the three. `gtd next --category <TEXT>` (repeated) SHALL keep the tasks carrying every category named, compared as the reserved values are. Each view SHALL be the predicate of pimdir GUIDE.md §17's query, which its `--json` output carries as `query`:

| View | Query |
| --- | --- |
| Inbox | `kind:task -status:completed -status:cancelled category:inbox` |
| Waiting | `kind:task -status:completed -status:cancelled category:waiting` |
| Someday | `kind:task -status:completed -status:cancelled category:someday` |
| Next | `kind:task -status:completed -status:cancelled -category:inbox -category:waiting -category:someday` |

Next SHALL be sorted by due date (undated last), then priority (1 first, unset last), then oldest captured first; the other views oldest captured first. A task's capture SHALL be its `CREATED`, else its `DTSTAMP`. Rows SHALL carry the account, the tasklist and a short id long enough to tell apart every `UID` of the scope, so every verb taking a task accepts it. The table SHALL show each task's short id, summary, due date, priority (as `task list` spells it), its own categories and tasklist, and its account when the scope spans several; Waiting SHALL also show each task's `CONTACT`s and how many whole days it has waited since its `DTSTAMP`, carried in `--json` as `waitedDays`. A listing an account could not reach SHALL carry it in `--json` as `unreached`.

#### Scenario: A to-do another client wrote is a next action
- GIVEN a task carrying no `STATUS` and `CATEGORIES:home`
- WHEN `unfog gtd next` runs
- THEN the task is listed

#### Scenario: Closed tasks are in no view
- GIVEN a completed task carrying `inbox` and a cancelled task carrying nothing
- WHEN each view runs
- THEN neither is listed

#### Scenario: A view's short id works everywhere
- GIVEN two tasks `ffff1` (in the Inbox) and `ffff2` (a next action)
- WHEN `unfog gtd inbox` runs
- THEN its short id for the first is `ffff1`, which `gtd clarify` accepts

### Requirement: The views span every account

A view, `gtd clarify`, `gtd review`, every `task` verb taking a task (`read`, `update`, `done`, `undone`, `delete`, `start`, `stop`), `task status`, `worktime` and `migrate` SHALL span every configured account, each through the backend `--backend` selects for it, and every tasklist of each; `-a/--account` SHALL narrow them to that account and `-l/--tasklist` to that tasklist. An account carrying no backend `--backend` allows SHALL be left out unless named. A task SHALL be found across that scope by its handle or a `UID` prefix, a prefix naming several tasks refused naming them.

An account that cannot be reached SHALL NOT fail a listing spanning several: the listing SHALL show what the others hold, warn on stderr naming the account and the error, and carry it in `--json` as `unreached: [{"account", "error"}]`. A verb writing one task SHALL fail when the task's account cannot be reached or when the task is not found and an account was unreached.

#### Scenario: One dead account
- GIVEN two accounts, one a CalDAV server refusing connections
- WHEN `unfog gtd next` runs
- THEN it lists the other account's next actions, warns about the dead one, and exits 0

### Requirement: The views on each backend

On CalDAV and vdir a view SHALL list each tasklist's VTODOs and apply its predicate. On pimdir it SHALL apply the predicate to the store's task summaries (`task_summary.status` and `categories`, the columns the guide's queries read, a column the store lacks read as empty), reading a body only for a task it keeps, deciding from the body a task a pending `update` repoints, and listing each pending `add` of a VTODO, so a staged capture or clarification shows at once. A pending add SHALL list under the handle `q<action id>`, with no version, and editing it SHALL be refused with the error code `pending-add` until a sync applied it. An item a pending `add` supersedes (a staged move) SHALL NOT be listed where it was, so a moved task shows once, where it goes. The pimdir backend SHALL NOT offer as a tasklist a collection whose `x-vtodo` capability is `none`.

#### Scenario: A staged clarification on pimdir
- GIVEN a pimdir store holding a task carrying `inbox`
- WHEN `unfog gtd clarify <TASK> --next` stages its update, and no sync ran
- THEN `unfog gtd inbox` no longer lists it and `unfog gtd next` does

#### Scenario: A staged capture on pimdir
- GIVEN a pimdir store
- WHEN `unfog gtd capture Call Bob` stages its add, and no sync ran
- THEN `unfog gtd inbox` lists it

### Requirement: Capture

`unfog gtd capture <TEXT>...` SHALL write a VTODO carrying a fresh `UID`, `DTSTAMP`, `CREATED` (the same instant), the text as `SUMMARY` and `CATEGORIES:inbox`, to the tasklist `-l` names, else `capture.tasklist`, else `tasklist.default`, else the tasklist the backend marks as the default, else the only one, of the account `-a` names or the default one. With no text it SHALL read stdin and refuse a terminal: an RFC 5322 message (its header block parses and carries `Subject` or `Message-ID`) gives its `Subject` as `SUMMARY` and its `Message-ID` as `ATTACH:mid:<id>` (RFC 2392), anything else its first line as the summary and the rest as the description.

### Requirement: Clarify

`unfog gtd clarify <TASK>` with one outcome flag (`--delete`, `--someday`, `--done`, `--waiting`, `--next`) SHALL clarify that task; `--summary` SHALL rephrase it, `--category` (repeated) add categories, `--due` set its due date, `--priority` its priority (`task update --priority`'s range), `--contact` (with `--waiting` only) replace its `CONTACT`s, written with `ALTREP="mailto:<address>"` when the value is or holds an e-mail address, `--parent <TASK>` name the task it is a step of (`RELATED-TO;RELTYPE=PARENT`), `--to <TASKLIST>` and `--to-account <ACCOUNT>` move it. With no outcome flag it SHALL walk the Inbox (or the task named) interactively, asking the app's steps: rephrase, actionable or not (Someday, delete, skip), done in under two minutes, someone else's (with whom), else a next action with categories, a parent task (optional), a due date and a tasklist; the interactive flow SHALL refuse `--json` and a non-terminal stdin, and list the scope once.

`--someday` and `--waiting` SHALL replace the reserved values by theirs, `--next` and `--done` SHALL remove them, `--done` also completing the task as `task done` does, and `--delete` SHALL delete it. A task already in the target state (`--someday` on a task carrying `someday` in any case, `--waiting` with no `--contact` on a waiting one, `--next` on an open task carrying no reserved value, `--done` on a completed one carrying none) SHALL be left untouched, no write and no new `DTSTAMP`, unless another flag changes it; its `--json` row SHALL carry `unchanged: true`. A writer SHALL edit only the three reserved values: the state lands where the first reserved value stood, else on a new `CATEGORIES` line after the last, which also takes the added categories; every other category SHALL keep its place, a `CATEGORIES` line holding no reserved value its bytes, and every other line its bytes but `DTSTAMP`. A recurring task SHALL be clarified on its master VTODO, its overrides kept as they are. Each edit SHALL be gated on the version read (`If-Match`).

A move SHALL create the task in the target and remove it from the source only once the create was accepted; on pimdir it SHALL stage one `add` carrying `supersedes` the source (pimdir STORAGE §15), so the source's tombstone is pushed only after the add was. A deleted or done task SHALL NOT be moved.

#### Scenario: A person's category is kept
- GIVEN a task carrying `CATEGORIES:work,Inbox,q3` and `CATEGORIES:phone-ish`
- WHEN `unfog gtd clarify <TASK> --next -C phone` runs
- THEN the task carries `CATEGORIES:work,q3`, `CATEGORIES:phone-ish` and `CATEGORIES:phone`, and every other line but `DTSTAMP` as it was

#### Scenario: A refused create keeps the capture
- GIVEN a task carrying `inbox`
- WHEN `unfog gtd clarify <TASK> --next --to <TASKLIST>` runs and the create in the target is refused
- THEN the command fails, the task is still in the Inbox where it was, and nothing is staged

#### Scenario: Already someday
- GIVEN a task carrying `someday`
- WHEN `unfog gtd clarify <TASK> --someday` runs
- THEN nothing is written

### Requirement: Review

`unfog gtd review` SHALL list the scope once, then print how many tasks Inbox, Next, Waiting and Someday hold, then each view in that order, then the waiting tasks waiting fourteen days or more (`waitingLong`) and, on pimdir, the waiting tasks whose `ATTACH:mid:` message got a reply (`replied`: a live mail of the store whose `In-Reply-To` names that `Message-ID`).

## ADDED Requirements

### Requirement: A plain category edit keeps the GTD state

`unfog task update <TASK> --category <TEXT>` (repeated) SHALL replace the person's own categories by those given, keeping the task's reserved value where it stood and every `CATEGORIES` line holding none of the person's old categories byte for byte; the reserved values SHALL NOT be given or removed through it. `task list --category` SHALL compare categories after the Unicode default lowercase mapping and a trim. A category containing `"` SHALL be refused before any listing or write.

#### Scenario: A waiting task keeps waiting
- GIVEN a task carrying `CATEGORIES:waiting,work`
- WHEN `unfog task update <TASK> --category home` runs
- THEN the task carries `CATEGORIES:waiting,home`

### Requirement: CalDAV writes reach the calendar named

On CalDAV unfog SHALL refuse, before any request, to address a calendar id that several calendars of the home share (a server keeping its calendars outside the home, as Google does, lists them all under one last segment), and SHALL refuse to create a VTODO in a calendar whose `supported-calendar-component-set` names components but not VTODO, saying it takes no tasks. A request meeting a connection the server closed (end of stream, reset, broken pipe) SHALL be sent once more over a new connection to the cached home-set.

#### Scenario: A capture into an events calendar
- GIVEN a CalDAV calendar stating `VEVENT` alone
- WHEN `unfog gtd capture -l <CALENDAR> Call Bob` runs
- THEN it fails saying the calendar takes no tasks, and the server holds nothing new
