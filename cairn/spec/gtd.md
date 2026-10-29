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

### Requirement: A plain category edit keeps the GTD state

`unfog task update <TASK> --category <TEXT>` (repeated) SHALL replace the person's own categories by those given, keeping the task's reserved value where it stood and every `CATEGORIES` line holding none of the person's old categories byte for byte; the reserved values SHALL NOT be given or removed through it. `task list --category` SHALL compare categories after the Unicode default lowercase mapping and a trim. A category containing `"` SHALL be refused before any listing or write.

#### Scenario: A waiting task keeps waiting
- GIVEN a task carrying `CATEGORIES:waiting,work`
- WHEN `unfog task update <TASK> --category home` runs
- THEN the task carries `CATEGORIES:waiting,home`

### Requirement: The command families

`capture`, `inbox`, `next`, `waiting`, `someday`, `clarify` and `review` SHALL be the `gtd` family, with no top-level alias. `start`, `stop` and `status` SHALL be subcommands of `task`, spanning every account as the GTD verbs do. `worktime` and `migrate` SHALL be top-level. Each SHALL hand the printer a named `*Output` type registered in the json-schema registry under its command path joined with hyphens (`unfog-gtd-next`, `unfog-task-start`, `unfog-worktime`, `unfog-migrate`), with an object root.
