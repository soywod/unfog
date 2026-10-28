---
cairn: log
change: gtd-family-and-task-tracking
landed: 2026-10-28
---

# A gtd family, time tracking under task, priority in the views

- **Command tree** (src/cli.rs, src/gtd/cli.rs, src/shared/task/cli.rs): `capture`, `inbox`, `next`, `waiting`, `someday`, `clarify` and `review` are the `gtd` family, no top-level alias; `start`, `stop` and `status` are `task` subcommands, still spanning every account; `worktime` stays at the root and `time migrate` becomes `migrate`, the `time` family gone. JSON schema keys follow the paths (`unfog-gtd-*`, `unfog-task-start|stop|status`, `unfog-migrate`).
- **Priority** (src/gtd/view.rs, src/gtd/clarify.rs): the views show a PRIORITY column as `task list` spells it, unsorted by it; `gtd clarify --priority <0-9>` sets it. The interactive flow does not ask it.

Spec: gtd (The views, Clarify, The views span every account, The command families replacing The GTD and time verbs are top-level) and time (every verb path) updated. rust-rewrite's delta.md command tree and proposal read the new paths; that change is not folded yet.
