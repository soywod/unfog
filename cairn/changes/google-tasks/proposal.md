---
cairn: change
id: google-tasks
status: active
created: 2026-10-29
---

# Tasks in Google Tasks

## Why

rust-rewrite planned a `gtasks` backend at its M4. Google stores no VTODO: gtd-hardening (2026-10-29) recorded that its CalDAV answers a VTODO PUT with 403, so a Google account holds tasks only through the Tasks API. Opened on its own by gtd-hardening.

## What

- A `gtasks` backend or family over the Tasks API: title, notes, a due date (time dropped), done and one level of subtasks.
- GTD on Google Tasks, which has no categories: how the reserved values are kept is the open question below.

## Out

- Time entries in a Google calendar over CalDAV: unfog cannot address a Google calendar but the primary yet (io-webdav names a calendar by its last href segment, `events` for every Google calendar), and Google drops RELATED-TO from a stored event, both recorded by gtd-hardening.

## Open questions

- **Google Tasks in the shared surface.** It holds so little that most field flags are refused there. Either it is a shared backend that refuses them, or it only gets its own `gtasks` family.
- **GTD without categories.** A task list per state (an `Inbox` list), a marker in the notes, or no GTD on Google Tasks.
