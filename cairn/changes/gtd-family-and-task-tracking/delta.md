---
cairn: delta
change: gtd-family-and-task-tracking
---

## MODIFIED Requirements

In spec/gtd.md: The views, Clarify, and The GTD and time verbs are top-level (renamed The command families). In spec/time.md: One running entry, Time strategies' scenario, Time migration. Every other requirement naming a GTD verb reads it under `unfog gtd`.

### Requirement: The views

`gtd inbox`, `gtd waiting` and `gtd someday` SHALL list the open tasks carrying that reserved value; `gtd next` SHALL list the open tasks carrying none of the three. `gtd next --category <TEXT>` (repeated) SHALL keep the tasks carrying every category named, compared as the reserved values are. Each view SHALL be the predicate of pimdir GUIDE.md §17's query, which its `--json` output carries as `query`:

| View | Query |
| --- | --- |
| Inbox | `kind:task -status:completed -status:cancelled category:inbox` |
| Waiting | `kind:task -status:completed -status:cancelled category:waiting` |
| Someday | `kind:task -status:completed -status:cancelled category:someday` |
| Next | `kind:task -status:completed -status:cancelled -category:inbox -category:waiting -category:someday` |

Rows SHALL be sorted as `task list` sorts them, and carry the account, the tasklist and a short id counted over the view. The table SHALL show each task's short id, summary, due date, priority (as `task list` spells it), its own categories and tasklist, and its account when the scope spans several.

### Requirement: Clarify

`unfog gtd clarify <TASK>` with one outcome flag (`--delete`, `--someday`, `--done`, `--waiting`, `--next`) SHALL clarify that task; `--summary` SHALL rephrase it, `--category` (repeated) add categories, `--due` set its due date, `--priority` its priority (`task update --priority`'s range), `--contact` (with `--waiting` only) replace its `CONTACT`s, `--to <TASKLIST>` and `--to-account <ACCOUNT>` move it. With no outcome flag it SHALL walk the Inbox (or the task named) interactively, asking the app's steps: rephrase, actionable or not (Someday, delete, skip), done in under two minutes, someone else's (with whom), else a next action with categories, a due date and a tasklist; the interactive flow SHALL refuse `--json` and a non-terminal stdin.

The rest of the requirement and its scenarios stand, the verb read as `unfog gtd clarify`.

### Requirement: The command families

`capture`, `inbox`, `next`, `waiting`, `someday`, `clarify` and `review` SHALL be the `gtd` family, with no top-level alias. `start`, `stop` and `status` SHALL be subcommands of `task`, spanning every account as the GTD verbs do. `worktime` and `migrate` SHALL be top-level. Each SHALL hand the printer a named `*Output` type registered in the json-schema registry under its command path joined with hyphens (`unfog-gtd-next`, `unfog-task-start`, `unfog-worktime`, `unfog-migrate`), with an object root.

### Requirement: One running entry

`unfog task start <TASK>` SHALL stop every running entry of every account first, and refuse a task already running. `unfog task stop` SHALL stop every running entry, `unfog task stop <TASK>` that task's alone and fail when it does not run; an event SHALL be stopped by its `DTEND` (and `DTSTAMP`), every other line kept, a journal by a `stop` VJOURNAL in the place holding its start. `unfog task status` SHALL list the running entries and since when.

#### Scenario: A start stops the running entry
- GIVEN an entry running for task A since 09:00, in another account
- WHEN `unfog task start B` runs at 10:00
- THEN A's entry ends at 10:00 and only B's runs

### Requirement: Time migration

`unfog migrate --to <STRATEGY>` SHALL copy every entry an account keeps anywhere but where that strategy writes, in its shape, to that place, and remove it from where it was only once the copy was accepted: the same bytes when the shape is kept, the other shape's spelling of the same span otherwise. It SHALL NOT change the configuration.

## REMOVED Requirements

### Requirement: The GTD and time verbs are top-level

Replaced by The command families.
