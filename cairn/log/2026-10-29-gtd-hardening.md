---
cairn: log
change: gtd-hardening
landed: 2026-10-29
---

# GTD and time tracking hardened across the stack

The cross-repo change of the night of 2026-10-28 and the day after (pimdir, io-pimdir, io-webdav, neverest, calendula, Android, unfog). unfog's part:

- **Time** (spec/time.md): an entry is a transparent VEVENT relating to a task of the scope, `DURATION` ending it as `DTEND` does, an unreadable end leaving it alone; a running entry always waits in the local place and a stop writes it finished where the strategy says; `migrate` keeps UIDs and resumes; `worktime` folds categories; `time.account`; the `local` strategy opens the store with `open_current`, never rebuilding it, retrying while a sync holds it.
- **GTD** (spec/gtd.md): every view and task verb spans every account and tasklist, `-a` and `-l` narrowing; an unreachable account is warned about and listed as `unreached`; on pimdir queued creates list as `q<id>`, editing one refused as `pending-add`, and a move is one add superseding the source; pimdir's writer vectors (GUIDE §17) run in unfog's tests; Waiting shows contacts and days waited, review lists long waits and replies, capture reads a piped mail; a plain category edit keeps the state.
- **CalDAV** (spec/backends.md): a calendar outside the home is addressed by its path, a closed connection is resent (both io-webdav's now), a task is refused into a calendar taking none, and a move within an account is a WebDAV `MOVE` then a write of the body, so Fastmail, keeping one UID per account, takes it; Google, answering `MOVE` with 404, gets a create then a remove.
- **Tests**: tests/radicale.sh, tests/caldav.rs (9 cases), tests/live.sh behind the guard proxy tests/live-guard.py (scratch collections only, the home listing filtered, a lent calendar and what it held read only), tests/pimdir-path.sh.

Capabilities moved: time (Time entries, Time strategies, One running entry, Worktime, Time migration), gtd (The views, The views span every account, The views on each backend, Capture, Clarify, Review; added A plain category edit keeps the GTD state), backends (added CalDAV writes reach the calendar named).

Verified on 2026-10-29: 160 unit tests and the CalDAV suite green; Radicale (waitress and its own server, closing after every response) 9/9; Fastmail 9/9 through the guard, the move landing under its resource name; Posteo 3 of 9 in the lent "test" calendar, the 6 needing calendars of their own skipped, the calendar left empty and in place; Google and Microsoft as the proposal's table records. The Posteo and Google cases left open continue in cairn/changes/provider-tests-posteo-google.
