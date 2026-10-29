---
cairn: spec
capability: time
status: current
---

# Time tracking

Time spent on a task is kept as time entries tied to it, in the shapes pimdir GUIDE.md §18 describes, where each account's `time.strategy` says. The model is src/time/entry.rs, the places src/time/place.rs, the verbs src/time/ (`task start`, `task stop`, `task status`, `worktime`, `migrate`).

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
