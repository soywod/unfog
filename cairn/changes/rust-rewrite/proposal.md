---
cairn: change
id: rust-rewrite
status: active
created: 2026-10-27
---

# Unfog in Rust: the tasks CLI

## Why

Unfog 1.0.6 (Haskell, about 1.8k lines) keeps tasks and time in one local append-only event log, readable by nothing else. Its rewrite is task 8 of the NLnet 2027 plan.

Pimalaya splits the iCalendar components by domain, one CLI each (decided 2026-10-27, after weighing a GTD tool, a tasks-plus-time tool and VTODO-embedded time logs):

- **calendula**: VEVENT (change events-only).
- **unfog**: VTODO.
- a **journal CLI** (working name): VJOURNAL.
- an **alarm daemon** firing the VALARMs of events and tasks.

The split follows the standards and the providers: one component type per CalDAV object (RFC 4791 4.1), and separate task services at Google (Tasks), Microsoft (To Do) and Apple (Reminders).

> Superseded for GTD and time tracking by cairn/changes/gtd-and-time-tracking (2026-10-28): unfog carries both.

**Workflows sit on top, not inside.** GTD and time tracking are opinions over the domains, built as front ends, scripts or MCP clients over unfog's and the journal CLI's `--json`. unfog's job is to make that possible: every property such a layer needs must be readable and writable through unfog. Time tracking leaves unfog: a script writes START and STOP entries with the journal CLI and computes durations; `TaskStarted`/`TaskStopped` users migrate through the import below.

## What

### Shape

calendula's, copied rather than shared (cli-shared-client-pattern): a `BackendClient` enum in src/shared/client.rs, one variant per backend behind a cargo feature, glue in src/<backend>/backend.rs, pimalaya-cli and pimalaya-config, the machine output contract (`--json` in camelCase, a per-binary `ErrorCode`, a json-schema registry with object roots, ready for MCP).

A task is a VTODO everywhere. Backends that speak iCalendar store its bytes; the others project it, as calendula's gcal backend projects a VEVENT (spec/projection.md there): managed fields mapped both ways, the rest stashed where the provider has a slot, refused where it has none.

### Backends

| Backend | Storage | Milestone |
| --- | --- | --- |
| CalDAV | VTODO in calendars whose `supported-calendar-component-set` takes it | M1 |
| vdir | VTODO files | M1 |
| pimdir | VTODO items, synced by neverest | M1 |
| msgraph | To Do `todoTask`, projected | M3 |
| gtasks | Google Tasks, projected | M4 |

### Commands

Singular commands, hidden plural aliases.

- `tasklist list|create|delete`: the collections. On CalDAV a calendar created with `VTODO` alone; on Graph a `todoTaskList`; on Google a task list.
- `task list [-l <tasklist>] [--status] [--due-before] [--category]`: open tasks first, due ones by date.
- `task read <id>`, with `--raw` for the exact bytes on iCalendar backends.
- `task create` and `task update`: an iCalendar source, the field flags and `-i/--interactive` (the composer) stacked in that order, as calendula's `event` family does. An update sends the entity tag it read as `If-Match`.
- `task done <id>` and `task undone <id>`: `STATUS:COMPLETED` with `COMPLETED` and `PERCENT-COMPLETE:100`, and back to `NEEDS-ACTION`. The common partial update deserves a verb.
- `task delete <id>`.
- `import <path>`: the Haskell store, see below.
- Backend families (`caldav`, `vdir`, `pimdir`, later `msgraph`, `gtasks`) for what the shared surface cannot carry.
- Meta: `configure` (wizard), `completion`, `manual`, `json-schema`.

A task is addressed by its handle, or by the shortest unique prefix of its `UID` as 1.x does.

### Field flags

`--summary`, `--description`, `--due`, `--start`, `--status`, `--priority`, `--category` (repeated), `--parent <uid>` (`RELATED-TO;RELTYPE=PARENT`), `--url`, and the alarm flags below. Each replaces every instance of what it names and leaves every other line byte for byte (calendula's rule). A flag a backend cannot hold is refused on that backend before the write, never dropped.

Anything else (`CONTACT`, `ATTACH`, other `RELATED-TO` types, X- properties such as `X-PIMALAYA-GTD`) goes through an iCalendar source or the composer. That is what lets a GTD layer live outside: read the bytes, patch them, write them back with `If-Match`.

### Alarms

- `--alarm <WHEN>`: one display alarm, either a lead before the due date (`15m`, `1d`, or an RFC 5545 duration, written as `TRIGGER;RELATED=END:-<LEAD>`) or an absolute time (`TRIGGER;VALUE=DATE-TIME`). A lead needs a `DUE`.
- `--no-alarm`: removes every VALARM.

CalDAV, vdir and pimdir store the VALARM. To Do holds one absolute `reminderDateTime`: a lead is resolved against the due date on write. Google Tasks holds none: refused. When an alarm fires is ical-rs's alarm-instants change; firing it is the alarm daemon's.

### Import of the Haskell store

`unfog import <store>` reads the 1.x event log (and its v0 format, as MigrationV0.hs does):

- `TaskAdded` and `TaskEdited` fold into one VTODO each; `Due` is `DUE`; the 1.x project becomes a `CATEGORIES` value.
- `TaskDid` is `STATUS:COMPLETED`; `TaskDeleted` is `STATUS:CANCELLED`, kept so its time stays linked.
- `ContextEdited` is dropped.
- `TaskStarted` and `TaskStopped` pairs become time entries of the account's `time.strategy` (cairn/changes/gtd-and-time-tracking, 2026-10-28), where this proposal first wrote them to a VJOURNAL file for a journal CLI.

### What 1.x users lose

`context` leaves unfog, replaced by `gtd next --category`. `start`, `stop`, `status` and `worktime` stay, brought back by cairn/changes/gtd-and-time-tracking (2026-10-28), the first three under `task` (cairn/changes/gtd-family-and-task-tracking), their entries kept where `time.strategy` says rather than in 1.x's store. The README says so first.

## Out

- Nothing of GTD and time tracking any more: cairn/changes/gtd-and-time-tracking (2026-10-28) brought both into unfog.
- Recurrence flags: `RRULE` through a source or the composer.
- A Vim plugin: unfog.vim speaks 1.x and stays there until ported.

## Open questions

- **Google Tasks in the shared surface.** It holds title, notes, a due date (time dropped), done and one level of subtasks, so most field flags are refused there. Either it is a shared backend that refuses them, or it only gets its own `gtasks` family. Decide at M4.
- **To Do stash.** Whether open extensions on `todoTask` can stash unmanaged properties (to verify at M3); without them, a To Do round trip is lossy beyond the managed fields.
- **`tasklist` or `list`** for the collection family: `list` is the user's word (Google, Microsoft and Apple all say "list") but reads oddly as `unfog list list`.
