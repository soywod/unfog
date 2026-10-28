---
cairn: delta
change: rust-rewrite
---

## ADDED Requirements

The requirements below open the spec of unfog 2.x, a VTODO task manager shaped as calendula is. They fold into four new capabilities: spec/commands.md (the command tree, the task model, the field flags, the alarms, the output contract), spec/backends.md (CalDAV, vdir, pimdir), spec/config.md (the TOML schema and the wizard) and spec/import.md (the 1.x store).

One component kind, The command tree, The task listing (what open means), Configuration, Import of the 1.x store and The time entries of an import read as cairn/changes/gtd-and-time-tracking modified them (2026-10-28), that change having landed first. The command tree reads as cairn/changes/gtd-family-and-task-tracking modified it again (2026-10-28).

### Requirement: One component kind, VTODO

unfog SHALL read and write VTODO components (RFC 5545 3.6.2) for tasks. A task listing SHALL keep the VTODOs of a collection and drop every other component kind. unfog SHALL write a VEVENT or a VJOURNAL only as a time entry (spec/time.md), never otherwise.

#### Scenario: A mixed collection
- GIVEN a collection holding one VTODO and one VEVENT
- WHEN `task list` runs on it
- THEN one task is listed, the VTODO

### Requirement: The command tree

The command tree SHALL be split into the `gtd` family (spec/gtd.md), the shared API (`tasklist`, `task` with its `start`, `stop` and `status`, `worktime`, `migrate`, `import`, spec/time.md for the time verbs), the backend families (`caldav`, `vdir`, `pimdir`, each behind its own cargo feature), and the meta commands (`configure`, `account`, `completion`, `manual`, `json-schema`), in that order. Commands SHALL be singular, each plural a hidden alias. The shared API SHALL expose only operations every compiled backend serves identically; a concept one backend lacks SHALL live in that backend's family.

1.x's `context` SHALL have no command of its own: `gtd next --category` replaces it.

### Requirement: Tasklists

`tasklist list` SHALL list the collections of the account that can hold VTODOs, `tasklist create <ID> <NAME>` SHALL create one, and `tasklist delete <ID>` SHALL delete one with every task it holds. On CalDAV a tasklist SHALL be created as a calendar whose `supported-calendar-component-set` holds `VTODO` alone, and `tasklist list` SHALL leave out a calendar whose set does not take `VTODO`.

The task commands SHALL take their tasklist from `-l/--tasklist`, else from the account's `tasklist.default`, else refuse.

#### Scenario: An events-only calendar
- GIVEN a CalDAV home holding a calendar whose component set is `VEVENT` alone and one whose set is `VEVENT` and `VTODO`
- WHEN `tasklist list` runs
- THEN only the second is listed

### Requirement: A task is addressed by its handle or a UID prefix

Every command taking a task SHALL accept the backend's handle of its item, or a prefix of its `UID`. A handle SHALL win over a prefix. A prefix SHALL be compared with the dashes of both sides removed, as 1.x does. A prefix matching no task SHALL be refused as not found, and one matching several SHALL be refused naming the matches, never guessed.

#### Scenario: A short prefix
- GIVEN a tasklist holding tasks of UIDs `4c1e0b2a-...` and `9f00...`
- WHEN `task done 4c1e` runs
- THEN the first task is completed

#### Scenario: An ambiguous prefix
- GIVEN tasks of UIDs `4c1e0b2a-...` and `4c1e77d0-...`
- WHEN `task read 4c1e` runs
- THEN the command fails naming both, and nothing is written

### Requirement: The task listing

`task list` SHALL list one row per VTODO: its short id, summary, due date, status, priority and categories. The short id SHALL be the dash-less `UID` prefix of the shortest length, from 4 up, that tells every listed task apart. Open tasks (any status but `COMPLETED` and `CANCELLED`, no status included) SHALL come before closed ones, and within each group tasks with a `DUE` SHALL come first, earliest first.

`--status <STATUS>` (repeated) SHALL keep the tasks of those statuses, `--due-before <DATE>` the tasks due before that day, and `--category <TEXT>` (repeated) the tasks carrying every category named. Under `--json` each task SHALL carry its handle `id`, its `uid`, its `shortId` and the fields above, `due` in its iCalendar wire spelling.

#### Scenario: Open first, by due date
- GIVEN a completed task due on the 1st, an open task due on the 3rd and an open task due on the 2nd
- WHEN `task list` runs
- THEN the rows read: due on the 2nd, due on the 3rd, the completed one

### Requirement: Reading a task

`task read <TASK>` SHALL print the task's fields, and with `--raw` the exact bytes the backend stores. Under `--json` it SHALL print the projection and the raw iCalendar as text.

### Requirement: A source, the field flags and the composer stack

`task create` and `task update` SHALL take an iCalendar source (a path, raw contents or `-` for stdin), the field flags and `-i/--interactive`, applied in that order: the source is the base, the flags set the properties they name on it, and the composer edits the result. With no source, `create` SHALL mint a VTODO carrying a fresh `UID` and a `DTSTAMP`, and `update` SHALL read the stored task and send the entity tag it read as `If-Match`. A command given none of the three SHALL be refused.

A write SHALL set the task's `DTSTAMP` to the instant of the write, and its `LAST-MODIFIED` too where it carries one.

### Requirement: The field flags

`--summary`, `--description`, `--due`, `--start`, `--status`, `--priority`, `--category` (repeated), `--parent <UID>` (`RELATED-TO;RELTYPE=PARENT`), `--url`, `--alarm` and `--no-alarm` SHALL each replace every instance of the property it names and leave every other line of the task byte for byte. An empty text value SHALL remove its property. `--due` and `--start` SHALL take `YYYY-MM-DD`, `YYYY-MM-DDTHH:MM[:SS]`, the same with a trailing `Z`, or a time prefixed by an IANA zone, which brings its `VTIMEZONE`. A flag SHALL apply to a source holding exactly one VTODO and be refused on any other.

A flag a backend cannot hold SHALL be refused on that backend before anything is written, never dropped.

#### Scenario: A flag edit keeps the rest
- GIVEN a task another client wrote, with folded lines, an `X-` property and a `VALARM`
- WHEN `task update <TASK> --summary "New title"` runs
- THEN the stored bytes differ from the original only in the `SUMMARY` line and the `DTSTAMP` line

### Requirement: Done and undone

`task done <TASK>` SHALL set `STATUS:COMPLETED`, `COMPLETED` to the instant of the write in UTC and `PERCENT-COMPLETE:100`. `task undone <TASK>` SHALL set `STATUS:NEEDS-ACTION` and remove `COMPLETED` and `PERCENT-COMPLETE`. Both SHALL leave every other line byte for byte and send the entity tag they read as `If-Match`.

#### Scenario: An unknown property survives done
- GIVEN a task carrying `X-PIMALAYA-GTD:next`
- WHEN `task done <TASK>` runs
- THEN the stored task carries `STATUS:COMPLETED`, a `COMPLETED`, `PERCENT-COMPLETE:100` and still `X-PIMALAYA-GTD:next`

### Requirement: Alarms

`--alarm <WHEN>` SHALL replace every `VALARM` of the task with one display alarm (`ACTION:DISPLAY`, its `DESCRIPTION` the task's summary or `Reminder`). A lead (`15m`, `1h`, `2d`, `1w`, or an RFC 5545 duration such as `PT15M`) SHALL be written as `TRIGGER;RELATED=END:-<DURATION>`, and SHALL be refused on a task carrying no `DUE` once the other flags are applied. A time, spelled as `--due` is, SHALL be written as `TRIGGER;VALUE=DATE-TIME` in UTC; a floating time SHALL be read in the local zone, and a bare date refused. `--no-alarm` SHALL remove every `VALARM`, and SHALL NOT be given with `--alarm`.

#### Scenario: A lead before the due date
- GIVEN a task due `20261030T170000Z`
- WHEN `task update <TASK> --alarm 15m` runs
- THEN the task carries one `VALARM` with `TRIGGER;RELATED=END:-PT15M`

#### Scenario: A lead without a due date
- GIVEN a task carrying no `DUE`
- WHEN `task update <TASK> --alarm 1h` runs
- THEN the command fails naming `--due`, and nothing is written

### Requirement: Deleting a task

`task delete <TASK>` SHALL delete the item holding the task, gated on the entity tag it read.

### Requirement: The output contract

Every command SHALL write its data and its errors to stdout, logs and prompts to stderr, the exit code being the only success signal. `--json` SHALL switch every command to JSON with camelCase keys. A command returning data SHALL hand the printer a named `*Output` type, every one registered in the json-schema registry with an object root. An error SHALL carry a code from unfog's own `ErrorCode` enum under `--json`, `precondition-failed` when an `If-Match` no longer holds, `body-pending` when a pimdir item is listed but not downloaded yet. The composer SHALL NOT be spawned under `--json`.

### Requirement: Backends

The CalDAV, vdir and pimdir backends SHALL store a task's iCalendar bytes verbatim and return them verbatim, as calendula's do. Without `-b/--backend`, the first configured backend SHALL win, in the order vdir, pimdir, CalDAV.

### Requirement: Configuration

The configuration SHALL be read from `$XDG_CONFIG_HOME/unfog/config.toml` (or the paths `-c/--config` or `UNFOG_CONFIG` name, deep-merged), as `[accounts.<name>]` blocks each carrying one block per backend (`vdir.home-dir`, `pimdir.root`, `caldav.*` as in calendula), `tasklist.default`, `task.composer`, `capture.tasklist`, `time.strategy`, `time.collection`, `time.directory` and the table options, each settable globally and per account. `unfog configure` SHALL run the account wizard, which SHALL also be offered when a command needs an account and none is configured.

### Requirement: Import of the 1.x store

`unfog import <STORE>` SHALL read a 1.x event log, one Haskell `Show` value per line, in the 1.x format (`TaskAdded`, `TaskEdited`, `TaskStarted`, `TaskStopped`, `TaskDid`, `TaskUndid`, `TaskDeleted`, `TaskUndeleted`, `ContextEdited`) or, when a line does not read in it, the v0 format (`TaskCreated`, `TaskUpdated`, `TaskStarted`, `TaskStopped`, `TaskMarkedAsDone`, `TaskUnmarkedAsDone`, `TaskDeleted`, `ContextSet`), as 1.x itself falls back. It SHALL open the store for reading only.

Events SHALL be applied in file order, and the events of one task SHALL fold into one VTODO: its 1.x id as `UID` (a fresh UUID for a v0 reference), the last description as `SUMMARY`, the last due date as `DUE` in UTC, the last project as one plain `CATEGORIES` value, no reserved GTD value added (v0 tags joined by `-`), the time of `TaskAdded` as `CREATED`. `TaskDid` SHALL give `STATUS:COMPLETED` with `COMPLETED` at its time, `TaskDeleted` `STATUS:CANCELLED`, and their undoing events SHALL put the task back to `NEEDS-ACTION`. `ContextEdited` and `ContextSet` SHALL be dropped. An event naming a task no `TaskAdded` introduced SHALL be skipped with a warning.

Each task SHALL be created in the selected tasklist of the selected backend. A task whose `UID` the tasklist already holds SHALL be skipped and counted, so an import run twice writes nothing the second time.

#### Scenario: A task added, edited, done
- GIVEN a store with `TaskAdded`, then `TaskEdited` changing its description and project, then `TaskDid`
- WHEN it is imported
- THEN one VTODO is written with the edited summary, the new project as its category, `STATUS:COMPLETED` and `COMPLETED` at the `TaskDid` time

#### Scenario: A deleted task
- GIVEN a store where a task was added then deleted
- WHEN it is imported
- THEN its VTODO carries `STATUS:CANCELLED`

### Requirement: The time entries of an import

`TaskStarted` and each `TaskStopped` closing it SHALL become one time entry of the account's `time.strategy` (spec/time.md), in its place and shape: an event `DTSTART` the start and `DTEND` the stop (none when the store ends with the task running), or a `start` and a `stop` VJOURNAL. A `TaskDid` or `TaskDeleted` closing a started task SHALL add the stop 1.x implied. An entry's `UID` SHALL be the task's `UID` and the start in Unix seconds (`-start` and `-stop` suffixed for a journal pair, the stop's instant in the second), and an entry whose start item the place already holds SHALL be skipped and counted, so an import run twice writes nothing the second time.

#### Scenario: A worked task
- GIVEN a store where a task was started at 09:00Z and stopped at 10:00Z
- WHEN it is imported under the default strategy
- THEN the local place holds one VEVENT from 09:00Z to 10:00Z related to the task's `UID`, and a second import adds none

## MODIFIED Requirements

None: unfog 1.x has no spec.

## REMOVED Requirements

None: unfog 1.x has no spec. The 1.x command `context` is gone, replaced by `gtd next --category` (cairn/changes/gtd-and-time-tracking, which also brought back `start`, `stop`, `status` and `worktime`, the first three under `task` since cairn/changes/gtd-family-and-task-tracking).
