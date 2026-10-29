---
cairn: spec
capability: backends
status: current
---

# Backends

Where unfog keeps tasks: CalDAV through io-webdav, a vdir directory, and a pimdir store through io-pimdir, each behind its own cargo feature (src/caldav/, src/vdir/, src/pimdir/).

### Requirement: Backends

The CalDAV, vdir and pimdir backends SHALL store a task's iCalendar bytes verbatim and return them verbatim, as calendula's do. Without `-b/--backend`, the first configured backend SHALL win, in the order vdir, pimdir, CalDAV.

### Requirement: CalDAV writes reach the calendar named

On CalDAV a calendar SHALL be addressed by the id io-webdav lists it under: the last segment of its href under the home, its path for one outside it (Google keeps each at `/caldav/v2/<calendar>/events/`), so a write reaches the calendar named. unfog SHALL refuse to create or move a VTODO into a calendar whose `supported-calendar-component-set` names components but not VTODO, saying it takes no tasks. A request meeting a connection the server closed SHALL be sent once more over a new connection (io-webdav's client does). A move within one account SHALL be a WebDAV `MOVE` of the task, then a write of its new body there, keeping its resource name; a server answering `MOVE` with 404, 405 or 501 (Google) SHALL see a create in the target, then the source removed once that create was accepted.

#### Scenario: A capture into an events calendar
- GIVEN a CalDAV calendar stating `VEVENT` alone
- WHEN `unfog gtd capture -l <CALENDAR> Call Bob` runs
- THEN it fails saying the calendar takes no tasks, and the server holds nothing new

#### Scenario: A task moved on a server keeping one UID per account
- GIVEN a task in calendar A of a Fastmail account, which refuses a create of a UID another calendar of the account holds
- WHEN `unfog gtd clarify <TASK> --next --to B` runs
- THEN the task is moved into B under its resource name with its clarified body, and A holds nothing
