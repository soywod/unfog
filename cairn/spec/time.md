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

When reading, a VEVENT SHALL be an entry when it carries a `RELATED-TO`, and a VJOURNAL half of one when it also carries `start` or `stop` (any case); the marks of a task SHALL pair in time order, a start left alone running and a stop with no start ignored. Times SHALL be written in UTC and read in their `TZID` zone, else the local one.

### Requirement: Time strategies

`time.strategy` SHALL say where `task start` writes, per account (or globally): `event` and `journal` in the tasklist `time.collection` names (refused when unset), `local` (the default) in the event shape on the device: the local time collection of the pimdir store (`U+0001time`, declared with `ensure_time_collection`, written as the store's owner with `put_item`, removed with `delete_unbound`, never queued) on the pimdir backend, else the vdir directory `time.directory` names, `$XDG_DATA_HOME/unfog/time` by default. The pimdir backend SHALL NOT list the local time collection as a tasklist.

#### Scenario: Each strategy round trips
- GIVEN an account of each strategy
- WHEN `unfog task start <TASK>` runs at 09:00 and `unfog task stop` at 10:30
- THEN `unfog worktime` reports 1h30 for the task, the entry an event with `DTEND` in the local place or in `time.collection`, or a `start` and a `stop` VJOURNAL in `time.collection`

### Requirement: One running entry

`unfog task start <TASK>` SHALL stop every running entry of every account first, and refuse a task already running. `unfog task stop` SHALL stop every running entry, `unfog task stop <TASK>` that task's alone and fail when it does not run; an event SHALL be stopped by its `DTEND` (and `DTSTAMP`), every other line kept, a journal by a `stop` VJOURNAL in the place holding its start. `unfog task status` SHALL list the running entries and since when.

#### Scenario: A start stops the running entry
- GIVEN an entry running for task A since 09:00, in another account
- WHEN `unfog task start B` runs at 10:00
- THEN A's entry ends at 10:00 and only B's runs

### Requirement: Worktime

`unfog worktime` SHALL sum every entry of every place each account keeps entries in (the local place and `time.collection`), whatever its strategy now, each entry once by `UID`, a running one up to now; `--since` and `--until` (a day at local midnight or an RFC 3339 instant) SHALL cut entries to that span, and `--category` (repeated) keep the tasks carrying every category named. It SHALL report the time per task, per category (the reserved values aside, a task counting in each of its categories) and in total.

#### Scenario: Worktime sums across strategies
- GIVEN 1h kept under `local`, then 30m under `event`, then 15m under `journal`
- WHEN `unfog worktime` runs under `journal`
- THEN it reports 1h45 in total

### Requirement: Time migration

`unfog migrate --to <STRATEGY>` SHALL copy every entry an account keeps anywhere but where that strategy writes, in its shape, to that place, and remove it from where it was only once the copy was accepted: the same bytes when the shape is kept, the other shape's spelling of the same span otherwise. It SHALL NOT change the configuration.
