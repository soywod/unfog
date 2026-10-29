---
cairn: change
id: provider-tests-posteo-google
status: active
created: 2026-10-29
---

# The Posteo and Google cases of the CalDAV suite

## Why

cairn/changes/gtd-hardening landed on 2026-10-29 with two provider boxes of its track E open (its tasks.md Notes, "Provisional (E, Posteo" and "Provisional (E, Google").

- **Posteo** creates no calendar over CalDAV (MKCALENDAR answers 403 "To create a calendar, please use the 'Settings' pages in Posteo's webmailer."). The user lends the run one calendar, "test" (VEVENT VTODO), and tests/live.sh borrows it: 3 of the 9 cases pass there, and the 6 needing a calendar of their own (a move between two calendars, both time strategies, the component check, the events-only refusal) skip.
- **Google**: io-webdav now addresses a secondary calendar by its path, an event lands in the calendar named, every calendar states VEVENT alone and `MOVE` answers 404 (io-webdav's own live test). unfog's suite authenticates with HTTP Basic only, so it has never run against Google itself.

## What

- Posteo: the cases needing a second calendar, once the user lends one more (a tasks calendar and an events or journal one), each borrowed as "test" is. No calendar created or deleted over CalDAV, nothing outside the lent calendars read or written.
- Google: a bearer token in the suite's `Server` (minted as io-webdav's tests/google.rs does, from the pimalaya.org service account), then the cases on scratch calendars made and deleted through the Calendar API: the task capture refused as "takes no tasks", the `event` strategy into a secondary calendar, RELATED-TO dropped, no ETag on PUT, a move falling back from the 404 to a create then a remove.

## Out

- The pimdir path on Posteo: neverest would list the account's other, personal calendar.
