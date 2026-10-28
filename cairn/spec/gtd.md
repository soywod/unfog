---
cairn: spec
capability: gtd
status: current
---

# GTD

unfog organises tasks the way pimdir GUIDE.md §17 states and the Pimalaya app applies: three reserved `CATEGORIES` values over plain VTODOs, so a task captured or clarified by one client reads the same in the other. The convention is src/shared/task/gtd.rs, the verbs src/gtd/, the scope across accounts src/scope.rs.

### Requirement: GTD state is three reserved categories

A task's GTD state SHALL be read from its `CATEGORIES`: `inbox`, `waiting` and `someday` are reserved, compared after the Unicode default lowercase mapping and a trim (pimdir STORAGE Annex A.2's rule for `fn`), and written as those English literals. Every other category SHALL be the person's own. unfog SHALL NOT read or write an X- property for GTD.

A task SHALL be open when its `STATUS` is anything but `COMPLETED` and `CANCELLED`, a task stating no `STATUS` included.

#### Scenario: A cased Inbox
- GIVEN an open task carrying `CATEGORIES:Inbox`
- WHEN `unfog gtd inbox` runs
- THEN the task is listed, and `unfog gtd next` does not list it

### Requirement: The views

`gtd inbox`, `gtd waiting` and `gtd someday` SHALL list the open tasks carrying that reserved value; `gtd next` SHALL list the open tasks carrying none of the three. `gtd next --category <TEXT>` (repeated) SHALL keep the tasks carrying every category named, compared as the reserved values are. Each view SHALL be the predicate of pimdir GUIDE.md §17's query, which its `--json` output carries as `query`:

| View | Query |
| --- | --- |
| Inbox | `kind:task -status:completed -status:cancelled category:inbox` |
| Waiting | `kind:task -status:completed -status:cancelled category:waiting` |
| Someday | `kind:task -status:completed -status:cancelled category:someday` |
| Next | `kind:task -status:completed -status:cancelled -category:inbox -category:waiting -category:someday` |

Rows SHALL be sorted as `task list` sorts them, and carry the account, the tasklist and a short id counted over the view. The table SHALL show each task's short id, summary, due date, priority (as `task list` spells it), its own categories and tasklist, and its account when the scope spans several.

#### Scenario: A to-do another client wrote is a next action
- GIVEN a task carrying no `STATUS` and `CATEGORIES:home`
- WHEN `unfog gtd next` runs
- THEN the task is listed

#### Scenario: Closed tasks are in no view
- GIVEN a completed task carrying `inbox` and a cancelled task carrying nothing
- WHEN each view runs
- THEN neither is listed

### Requirement: The views span every account

A view, `gtd clarify`, `gtd review`, `task start`, `task stop`, `task status`, `worktime` and `migrate` SHALL span every configured account, each through the backend `--backend` selects for it, and every tasklist of each; `-a/--account` SHALL narrow them to that account. An account carrying no backend `--backend` allows SHALL be left out unless named. A task SHALL be found across that scope by its handle or a `UID` prefix, a prefix naming several tasks refused naming them.

### Requirement: The views on each backend

On CalDAV and vdir a view SHALL list each tasklist's VTODOs and apply its predicate. On pimdir it SHALL apply the predicate to the store's task summaries (`task_summary.status` and `categories`, the columns the guide's queries read), reading a body only for a task it keeps, and deciding from the body a task a pending `update` repoints, so a staged clarification shows at once.

#### Scenario: A staged clarification on pimdir
- GIVEN a pimdir store holding a task carrying `inbox`
- WHEN `unfog gtd clarify <TASK> --next` stages its update, and no sync ran
- THEN `unfog gtd inbox` no longer lists it and `unfog gtd next` does

### Requirement: Capture

`unfog gtd capture <TEXT>...` SHALL write a VTODO carrying a fresh `UID`, `DTSTAMP`, the text as `SUMMARY` and `CATEGORIES:inbox`, to the tasklist `-l` names, else `capture.tasklist`, else `tasklist.default`, else the tasklist the backend marks as the default, else the only one, of the account `-a` names or the default one. With no text it SHALL read stdin, its first line the summary and the rest the description, and refuse a terminal.

### Requirement: Clarify

`unfog gtd clarify <TASK>` with one outcome flag (`--delete`, `--someday`, `--done`, `--waiting`, `--next`) SHALL clarify that task; `--summary` SHALL rephrase it, `--category` (repeated) add categories, `--due` set its due date, `--priority` its priority (`task update --priority`'s range), `--contact` (with `--waiting` only) replace its `CONTACT`s, `--to <TASKLIST>` and `--to-account <ACCOUNT>` move it. With no outcome flag it SHALL walk the Inbox (or the task named) interactively, asking the app's steps: rephrase, actionable or not (Someday, delete, skip), done in under two minutes, someone else's (with whom), else a next action with categories, a due date and a tasklist; the interactive flow SHALL refuse `--json` and a non-terminal stdin.

`--someday` and `--waiting` SHALL replace the reserved values by theirs, `--next` and `--done` SHALL remove them, `--done` also completing the task as `task done` does, and `--delete` SHALL delete it. A writer SHALL edit only the three reserved values: the state lands where the first reserved value stood, else on a new `CATEGORIES` line after the last, which also takes the added categories; every other category SHALL keep its place, a `CATEGORIES` line holding no reserved value its bytes, and every other line its bytes but `DTSTAMP`. Each edit SHALL be gated on the version read (`If-Match`).

A move SHALL create the task in the target and remove it from the source only once the create was accepted. A deleted or done task SHALL NOT be moved.

#### Scenario: A person's category is kept
- GIVEN a task carrying `CATEGORIES:work,Inbox,q3` and `CATEGORIES:phone-ish`
- WHEN `unfog gtd clarify <TASK> --next -C phone` runs
- THEN the task carries `CATEGORIES:work,q3`, `CATEGORIES:phone-ish` and `CATEGORIES:phone`, and every other line but `DTSTAMP` as it was

#### Scenario: A refused create keeps the capture
- GIVEN a task carrying `inbox`
- WHEN `unfog gtd clarify <TASK> --next --to <TASKLIST>` runs and the create in the target is refused
- THEN the command fails, the task is still in the Inbox where it was, and nothing is staged

### Requirement: Review

`unfog gtd review` SHALL print how many tasks Inbox, Next, Waiting and Someday hold, then each view in that order.

### Requirement: The command families

`capture`, `inbox`, `next`, `waiting`, `someday`, `clarify` and `review` SHALL be the `gtd` family, with no top-level alias. `start`, `stop` and `status` SHALL be subcommands of `task`, spanning every account as the GTD verbs do. `worktime` and `migrate` SHALL be top-level. Each SHALL hand the printer a named `*Output` type registered in the json-schema registry under its command path joined with hyphens (`unfog-gtd-next`, `unfog-task-start`, `unfog-worktime`, `unfog-migrate`), with an object root.
