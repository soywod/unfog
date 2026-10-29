---
cairn: change
id: microsoft-todo
status: active
created: 2026-10-29
---

# Tasks in Microsoft To Do

## Why

rust-rewrite planned an `msgraph` backend at its M3: Microsoft has no CalDAV, so a Microsoft 365 account reaches unfog only through Graph. Opened on its own by gtd-hardening (2026-10-29), which also made time entries able to live in another account's calendar (`time.account`), so this change is about tasks alone.

## What

- An `msgraph` backend projecting a VTODO onto a To Do `todoTask` (lists as `todoTaskList`), as calendula's gcal backend projects a VEVENT: managed fields mapped both ways, the rest stashed where Graph has a slot, refused where it has none.
- GTD on To Do: `todoTask.categories` holds the reserved `inbox`, `waiting` and `someday` as any other category (pimdir GUIDE.md §17), compared folded.
- Lead alarms resolved to `reminderDateTime`.

## Out

- Time entries in a Graph calendar: a calendar backend of its own. gtd-hardening recorded that Graph keeps RELATED-TO only in the `pimalaya-ical` extended property io-msgraph stashes an event's unmapped lines in.

## Open questions

- **To Do stash.** Whether open extensions on `todoTask` can stash unmanaged properties (the event stash of io-msgraph is a single-value extended property, which `todoTask` may not take); without one, a To Do round trip is lossy beyond the managed fields.
