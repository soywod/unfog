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

| Provider | Tasks (VTODO) | `event` entries | `journal` entries | State on 2026-10-28 |
| --- | --- | --- | --- | --- |
| Radicale (local) | yes | yes | yes | no account needed |
| Fastmail CalDAV | expected, unverified | expected | unverified | DAV password in pass |
| Posteo CalDAV | yes (both calendars state VEVENT VTODO) | yes | no (no calendar states VJOURNAL) | login checked, MKCALENDAR allowed |
| Google | not over CalDAV; Tasks API needs M4 (no categories) | primary or own calendar; PUT answers no ETag | no | delegated token minted; Calendar, CalDAV (207) and Tasks API read |
| Microsoft | no CalDAV; To Do needs M3 | needs a Graph calendar backend | no | app-only token minted; calendars and To Do lists read |

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

- Should GUIDE §17 allow a same-account CalDAV `MOVE` when a server enforces UID uniqueness across a calendar home? Answer from the Fastmail and Posteo tests.
- How GTD maps onto Google Tasks and Microsoft To Do (no categories in Google Tasks; To Do has categories): settled in M3 and M4.
