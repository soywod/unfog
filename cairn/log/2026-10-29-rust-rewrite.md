---
cairn: log
change: rust-rewrite
landed: 2026-10-29
---

# unfog 2.x, a VTODO task manager in Rust

Milestones M0 to M2 of the rewrite fold into four new capabilities, as later changes left them: spec/commands.md (one component kind, the command tree, tasklists, addressing a task, the listing, reading, the source with the field flags and the composer, the flags, done and undone, alarms, deleting, the output contract), spec/backends.md (the three backends), spec/config.md and spec/import.md (the 1.x store and its time entries).

Read as cairn/changes/gtd-and-time-tracking and cairn/changes/gtd-family-and-task-tracking (2026-10-28) and cairn/changes/gtd-hardening (2026-10-29) left them: a command taking a task finds it across the scope, `-l` and `tasklist.default` naming where a create goes; `--category` compared folded; `time.account` among the keys; `pending-add` among the error codes; the done scenario keeping an `X-` property another client writes, no longer `X-PIMALAYA-GTD`.

M3 (Microsoft To Do) and M4 (Google Tasks) left the change for cairn/changes/microsoft-todo and cairn/changes/google-tasks. Two boxes stay the user's: the repository's transfer to the pimalaya organisation with the Haskell sources on a 1.x branch, and the import checked on a copy of a real 1.x store.

Capabilities added: commands, backends, config, import.
