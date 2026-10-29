---
cairn: change
id: gtd-hardening
status: active
created: 2026-10-28
---

# Harden GTD and time tracking, then test them on real providers

## Why

A review of the GTD work on 2026-10-28 (pimdir GUIDE §17-18, io-pimdir, unfog, Android) found the convention right and the byte-level writers solid, but three weak seams: the pimdir store as shared truth (io-pimdir versions disagree, the phone writes no task summaries, queued creates are invisible), operations spanning a sync (moves, running time entries), and two writers of one convention that already disagree. unfog has no CalDAV test and nothing was ever run against a real provider. Every finding below was reproduced on scratch stores unless marked "by reading".

unfog is the reference; this change is the umbrella for the whole plan. Work in pimdir, io-pimdir and Android is listed here so one session sees it all; each of those repos gets its own cairn change when its track starts, pointing back here.

## What

### High

- **H1, io-pimdir version skew.** neverest and calendula lock io-pimdir 3b4bed8, Android 189db40, unfog builds ../io-pimdir by path (Cargo.toml:73). At 3b4bed8 `task_summary` lacks `priority`, `categories`, `parent`, `mid`; an owner reconciles to its own canonical schema and drops what it lacks (io-pimdir src/client/schema.rs:130); HEAD's reader names those columns unprobed (spec/queries/storage/read/list_tasks_page_asc.sql). Reproduced: a store whose `task_summary` has the 3b4bed8 shape makes `gtd inbox`, `task list` and `task start` fail with "no such column: s.priority". unfog's `local` time strategy also opens the store as owner (src/time/place.rs:217), which reconciles (rebuilds) a store that may hold a whole mailbox. Fix: one io-pimdir rev for every desktop owner, bumped together; the reader probes the task columns as STORAGE §6 requires (as it does for `collections.role` and `sources.covered_at`); unfog never becomes an owner implicitly.
- **H2, unfog rewrites events that are not its entries.** src/time/entry.rs `parse` takes any VEVENT with RELATED-TO in `time.collection` as an entry and ignores DURATION; a stop drops DURATION (entry.rs:350). Reproduced: a one-hour meeting (`DURATION:PT1H`, `RELATED-TO:agenda-doc-42`) shows as running for 31h48 and `task start` rewrites it with `DTEND:<now>`. Fix: DURATION is an end; an entry needs `TRANSP:TRANSPARENT` and a RELATED-TO naming a task of the scope; document that `time.collection` should be a calendar of its own.
- **H3, running entries staged on pimdir are invisible.** Under `event`/`journal` on a pimdir backend a start is a queued add (src/pimdir/backend.rs:296), which no listing shows: `task status` is empty, `task stop` says "Nothing runs", a second start opens another entry, and after a sync the next stop closes both at its own time (hours credited twice). By reading. Fix (decided default): a running entry always lives in the local place; at stop the finished entry is written where the strategy says, as one add.
- **H4, moves stage the remove before the add is performed.** GUIDE §17 (pimdir GUIDE.md:334) says remove "once the add was performed"; unfog over pimdir and Android (TaskWrites.java:104-108) stage both at once. A parked add (live duplicate link id, undeclared kind) or a refused push (events-only calendar, UID uniqueness) still lets the remove through. The pimdir test only covers a refusal at staging. By reading. Fix (decided default): pimdir STORAGE §15 gains an `add` carrying `supersedes: {collection, seq}`, applied with its tombstone in one owner transaction, the tombstone's push held until the add is pushed; io-pimdir implements it, both clients use it. Direct CalDAV in unfog is already correct.

### Medium

- **M1, the writers disagree.** Decided defaults, to be written into pimdir GUIDE §17 and checked by writer vectors (input VTODO, operation, expected bytes) that unfog and Android both run:

  | Rule | unfog today | Android today | Decided |
  | --- | --- | --- | --- |
  | Task already in the target state | rewrites the line | untouched (edit.rs:392) | untouched |
  | Plain category edit (`task update --category`) | drops the state (src/shared/task/fields.rs:307) | keeps it, collapses lines (edit.rs:251) | keeps the state; replaces only the person's categories, other lines byte-preserved |
  | Category filter | folded | exact (TasksList.java:373) | folded (Unicode lowercase + trim) |
  | Order | due date, ties in listing order | Next: due then priority (TasksList.java:466); others oldest first | Next: due, then priority, then oldest; other views oldest first |
  | CONTACT | text | `ALTREP=mailto:` given an address | `ALTREP=mailto:` given an address |
  | CREATED | never written | never written | written at capture; "captured at" reads CREATED, else DTSTAMP |

  The gtd-family proposal's "the app does not sort by it" is wrong and gets corrected in unfog's spec.
- **M2, the phone store keeps no task summaries.** CalendarEngine.java:213-216 keys items by resource name with "no key and no summary"; TaskWrites passes a null summary and a move gets `<random>.ics` (TaskWrites.java:107); TaskStore.java:73 parses every calendar object on every load; io-pimdir pin predates the task columns. Fix: bump io-pimdir, derive `task_summary` at listing and staging, read the lists from it.
- **M3, ids printed by the views do not work everywhere.** `task done|undone|update|delete|read` search one tasklist; views, `gtd clarify`, `task start|stop` the scope. Reproduced: `task done bbbb3` (id from `gtd next`) fails "Cannot find task `bbbb3` in tasklist `tasks`". View short ids are counted over the view only (src/gtd/view.rs:197); reproduced: `ffff` from `gtd inbox` refused by `clarify` as ambiguous. Fix: one scope resolver for every verb (`-a`/`-l` narrow it), short ids over every UID listed.
- **M4, one unreachable account hides all.** Reproduced: a dead CalDAV account makes `gtd next` exit 1 on "connect 127.0.0.1:9" (src/scope.rs:147). Fix: list what answers, warn about the rest, report `unreached` in JSON like neverest.
- **M5, queued creates invisible on pimdir.** `gtd inbox` right after a capture shows nothing; `clarify --to` hides the task everywhere until a sync. Fix: fold io-pimdir `pending_creates` into `select_tasks`; refuse editing a pending add with a clear error code.
- **M6, nothing knows a calendar refuses VTODO.** unfog over pimdir lists every `text/calendar` collection as a tasklist; Android's `takesTasks` offers Google CalDAV calendars. Fix: the owner declares per-collection support through STORAGE §15.6 (`x-vtodo` capability `none`), the producer gate refuses early, both clients read it.
- **M7, time tracking.** `migrate` uses fresh UUIDs (src/time/migrate.rs:114,123): not resumable, double-counts, and on pimdir deletes the local copy while the add is only queued; fix: keep the UID, skip entries already at the target. `worktime` keys categories by spelling (reproduced: "Work 1h00" and "work 30m"; src/time/worktime.rs:127); fix: fold. `tasklist create` makes VTODO-only calendars and place.rs:66 says "one of the account's tasklists"; fix: message names a calendar accepting VEVENT (or VJOURNAL). Add `time.account` so time entries can live in another account than tasks (Google, Microsoft). `local` on pimdir fails at once under a sync's owner lock; fix: short bounded retry, then a clear error.
- **M8, parity with Android.** `gtd waiting` shows CONTACT and age; `gtd review` adds "waiting over two weeks" and "reply received" (pimdir: `task_summary.mid` joined with `mail_summary.in_reply_to`); `gtd capture` from stdin parses an RFC 5322 message (Subject as summary, Message-ID as `ATTACH:mid:`); interactive clarify asks for a parent.

### Low

- One snapshot per command: `gtd review` lists the scope four times (src/gtd/review.rs:35), interactive clarify relists tasklists per item (src/gtd/clarify.rs:285).
- Recurring tasks with overrides cannot be clarified (`one_vtodo` refuses several VTODOs); edit the master like Android.
- `task list --category` compares ASCII case only (src/shared/task/list.rs:129).
- A category containing `"` has no SEARCH escape; refuse it before listing.
- Stale text: pimdir STORAGE.md:620 ("the bucket an organising client keeps"), GUIDE.md:333 ("through the owner's write path"), rust-rewrite proposal's `X-PIMALAYA-GTD` example.
- unfog cairn: fold rust-rewrite M0 to M2 into spec/, give M3 (To Do) and M4 (Google Tasks) their own changes.
- GUIDE §17 forbids `move` without saying why; decide after testing UID uniqueness on Fastmail and Posteo.
- unfog's git index holds a stale snapshot (src/gtd/cli.rs untracked, src/time/cli.rs staged but deleted, .direnv/bin/nix-direnv-reload staged, .direnv/ not ignored): re-stage before any commit, and add `.direnv/` to .gitignore.

### Provider tests

unfog has no CalDAV test. Reuse journal/tests/radicale.sh and tests/caldav.rs (Radicale from nixpkgs, `--ignored` tests pointed at it by an env URL), neverest/tests (radicale.sh, caldav.rs, google.rs, msgraph.rs, token minting in tests/common/mod.rs) and Android's e2e suite.

| Provider | Tasks (VTODO) | `event` entries | `journal` entries | State (2026-10-28) and results (2026-10-29) |
| --- | --- | --- | --- | --- |
| Radicale (local) | yes | yes | yes | 2026-10-29: tests/caldav.rs 9/9 (capture, foreign bytes kept but CATEGORIES and DTSTAMP, 412 on a stale write, move, both strategies, meeting untouched, early refusal into an events calendar); answers `Connection: close` after a PUT, which unfog now reconnects past; refuses a second resource of one UID in a calendar (409); pimdir path (tests/pimdir-path.sh): clarify pushed, a superseding move pushes its add then its held delete on the next sync |
| Fastmail CalDAV | yes | yes, RELATED-TO and TRANSP kept, a meeting beside untouched | yes (MKCALENDAR of a VJOURNAL calendar accepted and stated) | 2026-10-29 through the guard: tests/caldav.rs 9/9, guard refused nothing; one UID per calendar home: a create-then-remove move answers 403 "WebDAV collection already holds a resource with the same UID", unfog leaves the task where it was; pimdir path: the superseding add is refused the same way and the source stays after two more syncs (the hold works). Later on 2026-10-29: a move is now a WebDAV `MOVE` then a write of the body, and tests/caldav.rs 9/9 again, the move landing under its resource name |
| Posteo CalDAV | yes: capture, foreign bytes kept, 412 on a stale write, in the calendar the user lends the run ("test", VEVENT VTODO) | untested: needs a calendar beside the tasks | no calendar states VJOURNAL (2026-10-28) | 2026-10-29: MKCALENDAR answers 403 Sabre_DAV_Exception_Forbidden "To create a calendar, please use the 'Settings' pages in Posteo's webmailer." (DELETE of a calendar likewise, "To delete a calendar..."); nothing created, no existing calendar read or written. Later on 2026-10-29, the account's own "test" calendar lent to the run (token `pimalaya/posteo`): tests/live.sh with `UNFOG_E2E_CALENDAR`, the guard keeping that calendar and anything it held read only, 3 of the 9 cases run and pass, the 6 needing calendars of their own skipped, the calendar left empty and in place |
| Google | no: a VTODO PUT answers 403 with an empty `<D:error/>`; unfog now refuses it first ("takes no tasks") | io-webdav now names a secondary calendar by its path (`/caldav/v2/<calendar>/events`), and an event written there lands there (io-webdav tests/google.rs, two scratch calendars); before, every calendar listed as id `events`, addressing the primary calendar (a write was caught by the guard); raw PUT 201 with no ETag, renamed to `<UID>.ics` (Location), RELATED-TO dropped, TRANSP kept | no | 2026-10-29: scratch calendars made and deleted through the Calendar API; every calendar states the component set VEVENT alone; `MOVE` answers 404, so a move falls back to a create then a remove |
| Microsoft | no CalDAV; To Do needs M3 | needs a Graph calendar backend; Graph keeps RELATED-TO only in the `pimalaya-ical` extended property io-msgraph stashes (verbatim, read back with `$expand`), `showAs: free` for TRANSP, its own `iCalUId` | no | 2026-10-29: app-only token, scratch calendar made and deleted, one event read back |

Every live test works only in collections it creates, named `unfog-e2e-<run>-<n>`, refuses any request outside them, and deletes them however the run ends (plus leftovers of older runs carrying the prefix). It never reads or writes an existing collection, never syncs or migrates a real account.

Credentials (never print a value; load into a shell variable):

- Fastmail: login `pimalaya@fastmail.org` (Android e2e default), DAV password `pass show pimalaya/fastmail-webdav` (one line).
- Posteo: login `pass show posteo/email`, password first line of `pass show posteo`, server https://posteo.de:8443 (principal by `current-user-principal`, then `calendar-home-set`). Scratch calendars only: the user's existing calendars must never be read or written.
- Google: service-account key file (path in the night runbook, outside the repo), subject `google@pimalaya.org`, scopes `https://www.googleapis.com/auth/calendar https://www.googleapis.com/auth/tasks`, RFC 7523 JWT-bearer grant (domain-wide delegation; Calendar and Tasks APIs enabled). unfog takes it through `caldav.auth.bearer.token.command`; ortie has no JWT-bearer grant with a subject (only client credentials with a JWT client assertion), neverest's tests mint it with io-oauth `rfc7523`.
- Microsoft: app-only client credentials, `pass show pimalaya/test/microsoft` (line 1 the client secret, line `app id:` the client id), tenant resolved from https://login.microsoftonline.com/pimalaya.onmicrosoft.com/v2.0/.well-known/openid-configuration (`issuer`), scope `https://graph.microsoft.com/.default`, user `microsoft@pimalaya.onmicrosoft.com` addressed as `/users/<upn>` (app-only cannot use `/me`). Roles granted: `Mail.ReadWrite`, `Contacts.ReadWrite`, `User.Read.All`, `Calendars.ReadWrite`, `Mail.Send`, `Tasks.ReadWrite.All`. `pimalaya/microsoft-entra` is another app with no service principal in this tenant: do not use it for tests.

## Out

- unfog M3 (Microsoft To Do) and M4 (Google Tasks) themselves: their own changes. Live tests on those two providers stop at time entries in calendars and at the "no task calendar" paths.
- Any release, version bump or publish.
- A device run of Android's Tasks tab.

## Open questions

- Should GUIDE §17 allow a same-account CalDAV `MOVE` when a server enforces UID uniqueness across a calendar home? Answer from the Fastmail and Posteo tests. Answered on 2026-10-29 (provisional, for the user to decide): Fastmail enforces it, so a create-then-remove move between two calendars of one account can never succeed there (unfog fails whole, the task kept; over pimdir the superseding add parks and the source stays). Posteo could not be tested (no scratch calendar). Recommendation: allow a same-account WebDAV `MOVE` (RFC 4918 9.9) after the in-place edit, falling back to create-then-remove across accounts; not implemented tonight. Implemented later on 2026-10-29: a same-account move is a `MOVE` then a write of the body (io-webdav `move_item`), on CalDAV directly and, over pimdir, as the delivery of an add superseding an item of its own identity (pimdir SYNC §3 and §4, `list_superseded_bindings`); a server answering `MOVE` with 404, 405 or 501 (Google) gets a create then a remove; passes on Fastmail and Radicale.
- How GTD maps onto Google Tasks and Microsoft To Do (no categories in Google Tasks; To Do has categories): settled in M3 and M4.
