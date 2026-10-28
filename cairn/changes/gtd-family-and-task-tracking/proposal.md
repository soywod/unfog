---
cairn: change
id: gtd-family-and-task-tracking
status: landed
created: 2026-10-28
---

# A gtd family, time tracking under task, priority in the views

## Why

gtd-and-time-tracking (2026-10-28) put twelve verbs at the top level. Some are too generic to own it: `status` reads as a task's `STATUS` though it lists running time entries, `next` and `review` say nothing of GTD. The top level is better kept for what serves both with and without GTD; the GTD verbs are one opinion over tasks and read best as a family.

Starting, stopping and listing running work is about a task, so it belongs under `task`. `worktime` reports over every task and `migrate` moves entries between strategies: neither is about one task, so both stay at the root.

GTD stores no priority: choosing a next action goes by context, time, energy, then priority, a judgment. Yet that last criterion needs to be visible to be applied, and a manual `PRIORITY` serves with and without GTD. VTODO carries it, `task update --priority` writes it and `task list` shows it; the views and `clarify` do not.

## What

- `unfog gtd {capture,inbox,next,waiting,someday,clarify,review}`, no top-level alias; JSON schema keys `unfog-gtd-<verb>`.
- `unfog task {start,stop,status}`, spanning every account as before; JSON schema keys `unfog-task-<verb>`.
- `unfog worktime` stays; `unfog time migrate` becomes `unfog migrate`, JSON schema key `unfog-migrate`. The `time` family goes.
- The views show a PRIORITY column, between DUE and CATEGORIES, as `task list` spells it (1 to 9, 0 and unset blank). The sort stays `task list`'s: a priority is not an order, and the app does not sort by it.
- `gtd clarify --priority <0-9>` sets `PRIORITY` with the outcome, as `task update --priority` does. The interactive flow does not ask it: clarifying stays quick, and priority is decided last.

## Out

- Sorting or filtering the views by priority.
- Spelling priorities as bands (high, medium, low): `task list` shows the number, the views follow it.
- Shell aliases for the old verbs: a person's own (`alias gtd='unfog gtd'`).

## Open questions

None: settled with the user on 2026-10-28.
