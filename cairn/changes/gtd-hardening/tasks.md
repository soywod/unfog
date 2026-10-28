---
cairn: tasks
change: gtd-hardening
---

# Tasks

Tracks run in this order; a track in another repo opens its own cairn change there (proposal, tasks, delta) pointing back to this one. Each box is checked only after fmt, clippy, tests and the repo's verify pass.

## 0. Pre-flight

- [ ] `git status` in pimdir, io-pimdir, unfog, android, neverest, calendula; build and test each as it is, so later failures are ours
- [ ] delta.md for this change (unfog's spec/gtd.md and spec/time.md moves)

## A. io-pimdir and pimdir (H1, H4, M6, M1 rules)

- [ ] io-pimdir: the reader probes `task_summary` columns (`priority`, `categories`, `parent`, `mid`) and reads a missing one as NULL (STORAGE §6); test on a store with the 3b4bed8 `task_summary` shape (fails before the fix)
- [ ] pimdir STORAGE §15: `add` carrying `supersedes: {collection, seq}`, one owner transaction, tombstone push held until the add is pushed; invariants and vectors
- [ ] io-pimdir: implement `supersedes` in mutate.rs and the drain; tests for a parked add (duplicate link id) and a refused push leaving the source in place
- [ ] pimdir STORAGE §15.6: per-collection `x-vtodo` capability; producer gate refuses an add of a VTODO into a collection declaring `none`; io-pimdir implements it
- [ ] pimdir GUIDE §17: the M1 rules (proposal table), CREATED at capture, capture through the queue (fix GUIDE.md:333), move via `supersedes` (GUIDE.md:334)
- [ ] pimdir: writer vectors (input VTODO, operation, expected bytes) for set-state, already-in-state, plain category edit, foreign bytes kept, CONTACT with ALTREP, CREATED at capture; checks/vectors.py validates their shape
- [ ] pimdir STORAGE.md:620 Annex A.4 wording
- [ ] `nix flake check path:$PWD` in pimdir, `cargo test --all-features` in io-pimdir

## B. Pin alignment (H1)

- [ ] With a git grant: commit and push pimdir and io-pimdir, pin unfog, neverest and calendula to that rev (unfog drops the path dependency at Cargo.toml:73)
- [ ] Without a grant: unfog keeps the path; neverest and calendula get an uncommitted `[patch]` to ../io-pimdir for local builds only, listed in the morning report
- [ ] unfog never opens a store as owner implicitly (src/time/place.rs:217): the `local` strategy on pimdir writes through the unbound path without reconcile, or refuses with a clear message

## C. unfog time (H2, H3, M7)

- [ ] H2: DURATION is an end; an entry needs `TRANSP:TRANSPARENT` and a RELATED-TO naming a scope task; regression test with the meeting fixture (fails before)
- [ ] H3: running entries always local; stop writes the finished entry per strategy as one add; tests for status, stop and a second start before any sync
- [ ] M7: `migrate` keeps UIDs and skips entries already at the target; resumable test
- [ ] M7: `worktime` folds categories (regression: "Work" and "work" one row)
- [ ] M7: place.rs:66 message; `time.account`; bounded retry on the owner lock

## D. unfog GTD (M1, M3, M4, M5, M8, low)

- [ ] M1: already-in-state untouched; `task update --category` keeps the state; ordering per the rules; CONTACT ALTREP; CREATED at capture; run pimdir's writer vectors
- [ ] M3: one scope resolver for every task verb; short ids over every UID listed (regressions from the review: `task done <id from gtd next>`, `clarify ffff`)
- [ ] M4: partial results, warning, `unreached` in JSON
- [ ] M5: pending creates in `select_tasks`; editing a pending add refused with a clear code
- [ ] M8: waiting shows CONTACT and age; review adds waiting over two weeks and reply received; stdin RFC 5322 capture; parent in interactive clarify
- [ ] Low: one snapshot per command; master-only clarify of recurring tasks; Unicode folding in `task list --category`; `"` in a category refused early
- [ ] spec/gtd.md, spec/time.md drafts in delta.md; README, config.sample.toml, CHANGELOG

## E. unfog provider tests

- [ ] tests/radicale.sh and tests/caldav.rs on journal's pattern (ignored, env URL): capture, foreign task round trip (only CATEGORIES and DTSTAMP change), 412 on a stale If-Match, move, both time strategies, H2 meeting untouched
- [ ] Live harness: scratch collections `unfog-e2e-<run>-<n>` only, every request URL checked against them, cleanup however the run ends, leftovers swept by prefix
- [ ] Fastmail: read-only component check, then the Radicale cases plus byte preservation, two CATEGORIES lines kept, same UID in two calendars during a move, RELATED-TO and TRANSP kept, VJOURNAL accepted or not
- [ ] Posteo: same cases as Fastmail (no VJOURNAL expected)
- [ ] Google: CalDAV task capture refused with a clear message; `event` entries in a scratch calendar; RELATED-TO survival; no ETag on PUT handled
- [ ] Microsoft: tokens and calendars only until M3; record what a Graph calendar keeps of RELATED-TO
- [ ] pimdir path: neverest on the same io-pimdir rev syncs a scratch calendar into a scratch store (never ~/.local/state/neverest), unfog clarifies over it, sync, server state checked
- [ ] Results written into the proposal's provider table and the open question on `MOVE`

## F. Android (M1, M2, H4, M6)

- [ ] Its own cairn change; bump io-pimdir; derive `task_summary` at listing and staging; read lists from it
- [ ] M1 rules and the pimdir writer vectors in rust/src/calendar tests
- [ ] Moves through `supersedes`; capture targets from the `x-vtodo` capability
- [ ] E2eTaskTest on `fastmail-caldav` (capture, clarify, file elsewhere, send, wait); Gradle unit tests

## G. cairn hygiene

- [ ] Fold rust-rewrite M0 to M2 into unfog spec/; open M3 and M4 as their own changes
- [ ] rust-rewrite proposal: drop the `X-PIMALAYA-GTD` example; gtd-family spec: correct "the app does not sort by it"
- [ ] unfog .gitignore: `.direnv/`

## Notes
