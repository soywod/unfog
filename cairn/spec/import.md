---
cairn: spec
capability: import
status: current
---

# Import of the 1.x store

Reading a unfog 1.x store into tasks and time entries (src/import.rs, src/import/).

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
