---
cairn: spec
capability: commands
status: current
---

# Commands

unfog 2.x is a VTODO task manager shaped as calendula is: one component kind, a command tree split into families, field flags over a source and a composer, and the machine output contract of the Pimalaya CLIs. The verbs live in src/shared/task/ and src/shared/tasklist/, the error codes in src/error.rs and the json-schema registry in src/json_schema.rs.

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

A command creating a task SHALL take its tasklist from `-l/--tasklist`, else from the account's `tasklist.default`, else refuse. A command taking a task SHALL find it across every account and tasklist (spec/gtd.md, The views span every account), `-l` narrowing it to one tasklist.

#### Scenario: An events-only calendar
- GIVEN a CalDAV home holding a calendar whose component set is `VEVENT` alone and one whose set is `VEVENT` and `VTODO`
- WHEN `tasklist list` runs
- THEN only the second is listed

### Requirement: A task is addressed by its handle or a UID prefix

Every command taking a task SHALL accept the backend's handle of its item, or a prefix of its `UID`, searched across the scope it spans (spec/gtd.md). A handle SHALL win over a prefix. A prefix SHALL be compared with the dashes of both sides removed, as 1.x does. A prefix matching no task SHALL be refused as not found, and one matching several SHALL be refused naming the matches, never guessed.

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

`--status <STATUS>` (repeated) SHALL keep the tasks of those statuses, `--due-before <DATE>` the tasks due before that day, and `--category <TEXT>` (repeated) the tasks carrying every category named, compared Unicode-folded (pimdir GUIDE.md §17). Under `--json` each task SHALL carry its handle `id`, its `uid`, its `shortId` and the fields above, `due` in its iCalendar wire spelling.

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
- GIVEN a task carrying `X-OTHER-CLIENT-STATE:kept`, a property another client writes
- WHEN `task done <TASK>` runs
- THEN the stored task carries `STATUS:COMPLETED`, a `COMPLETED`, `PERCENT-COMPLETE:100` and still `X-OTHER-CLIENT-STATE:kept`

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

Every command SHALL write its data and its errors to stdout, logs and prompts to stderr, the exit code being the only success signal. `--json` SHALL switch every command to JSON with camelCase keys. A command returning data SHALL hand the printer a named `*Output` type, every one registered in the json-schema registry with an object root. An error SHALL carry a code from unfog's own `ErrorCode` enum under `--json`, `precondition-failed` when an `If-Match` no longer holds, `body-pending` when a pimdir item is listed but not downloaded yet, `pending-add` when an edit names a pimdir create a sync has not applied yet. The composer SHALL NOT be spawned under `--json`.
