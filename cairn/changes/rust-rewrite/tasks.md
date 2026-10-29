---
cairn: tasks
change: rust-rewrite
---

# Tasks

## M0: design

- [x] Repo moved to ~/code/pimalaya/unfog (2026-10-27)
- [x] Scope settled: tasks only, GTD and time tracking outside (2026-10-27)
- [ ] Repo transferred to the pimalaya GitHub org, Haskell sources kept on a 1.x branch (user)
- [x] cairn.toml, cairn/verify.sh and AGENTS.md copied from calendula
- [x] delta.md: the requirements and scenarios of this proposal, against a new spec (commands.md, backends.md, config.md, import.md)

## M1: tasks on iCalendar backends

- [x] Cargo crate at the repo root beside the Haskell sources (src/ is free: 1.x uses lib/, bin/, test/), shaped on calendula: Cargo.toml features and patches, flake.nix, shell.nix, default.nix, package.nix, build.rs, LICENSE-MIT, LICENSE-APACHE
- [x] Shared item layer and `BackendClient` for CalDAV, vdir and pimdir, ported from calendula
- [x] `task` family from calendula's src/shared/todo, extended: field flags, `done`, `undone`, short UID prefixes
- [x] `tasklist` family; CalDAV calendars created with `VTODO` alone
- [x] `--alarm` and `--no-alarm`
- [x] Output contract: `*Output` types, `ErrorCode`, json-schema registry with object roots
- [x] Wizard (pimalaya-cli `wizard`), config.sample.toml
- [x] Tests: a task another client wrote round trips byte for byte after a flag edit; an unknown X- property survives `done`
- [x] Each verb's `--help` in a debug build (clap collisions)

## M2: import

- [x] `import` of the 1.x store and its v0 format, tasks written to the configured backend
- [x] The VJOURNAL file of START and STOP entries
- [ ] Checked on a copy of a real 1.x store in the scratchpad, never the original

## M3: Microsoft To Do

Moved to its own change, cairn/changes/microsoft-todo (2026-10-29).

## M4: Google Tasks

Moved to its own change, cairn/changes/google-tasks (2026-10-29).

## M5: release

- [ ] README (what 1.x users lose first), CHANGELOG, MIGRATION.md, packaging, AI policy link
- [ ] Fold into spec/, log entry, change archived

## Notes

Provisional answers taken during the unattended session of 2026-10-27, to review before the fold.

- No real 1.x store exists on this machine (`$XDG_CONFIG_HOME` unset, ~/.config/unfog absent), so the last M2 box stays open; the import is checked on synthetic stores built from the Haskell `Show` format, tests/fixtures/store-1.x and tests/fixtures/store-0.x.
- The collection family is `tasklist`, as the proposal writes it (its open question stays open). Its selector is `-l/--tasklist`, falling back to `tasklist.default`; the composer is `task.composer`.
- `account list` and `account check` are kept from calendula beside the proposal's meta commands; the wizard reuses the check. There is no `item` family and no `task build`.
- A UID prefix is compared with dashes removed and case kept, as 1.x did, but an ambiguous prefix is refused naming the matches where 1.x took the first. A handle wins over a prefix. The short ids are at least 4 characters and counted over the whole tasklist, not the filtered view, so a short id read from a filtered listing still names one task.
- `task list` shows every status, open first, then by due date (the wall-clock spelling of `DUE`, its zone set aside), where 1.x hid done and deleted tasks; `--status` (repeated) narrows. `--due-before` is exclusive, `--category` repeated means all of them.
- An empty `--due ""` or `--start ""` removes the property, where calendula refuses an empty time: dropping a due date is a common task edit.
- No `--uid`, `--time-zone` or `--percent-complete` flag: a zone is a prefix of the time (`Europe/Paris:2026-10-30T17:00`). `--status completed` sets the status alone; `task done` also stamps `COMPLETED` and `PERCENT-COMPLETE`.
- `--alarm`: the display alarm's `DESCRIPTION` is the task's summary, `Reminder` when it has none; a lead also takes seconds (`90s`) and refuses zero; an absolute time naming no zone is read in the local zone, a bare date refused; a lead spelled with a leading minus must be passed as `--alarm=-PT15M` (clap reads `-P...` as a flag otherwise).
- Every write sets `DTSTAMP` (and an existing `LAST-MODIFIED`) to the instant of the write, a create from a source included, where calendula's `event create` sends a source untouched.
- `task update`, `done`, `undone` and `delete` find the task first and gate the write on the entity tag found; there is no `--if-match` flag. The JSON error code of a stale version is calendula's `precondition-failed`, not a new one.
- `import`: `--journal <PATH>` is optional and refused when the file exists (checked before any write); without it the time entries are counted and left out with a warning. A task both done and deleted imports as `CANCELLED` without `COMPLETED`. `CREATED` and `LAST-MODIFIED` come from the 1.x event times. Blank lines are skipped; an event naming a task never added is skipped with a warning (1.x fails).
- Re-running an import is idempotent for a 1.x store (its UIDs are kept) but not for a v0 one: v0 numbers get fresh UUIDs on every run, as 1.x's own migration does. On pimdir a created task lists only once a sync applied it, so an import re-run before a sync duplicates.
- unfog 2.x reads its configuration from ~/.config/unfog/config.toml, the path where 1.x kept a `store-path` config: a 1.x config file fails to load in 2.x (no `accounts`). For MIGRATION.md at M5.
- The crate version is `2.0.0-dev`, nothing being published. The `io-gcal` and `io-msgraph` patch entries are kept as calendula has them, though unused tonight (cargo warns).
- The flake is untracked, so `nix develop` refuses it: the devshell was built once with `nix develop path:$PWD --profile <scratchpad>` before target/ existed, and every cargo run went through that saved profile.
- 2026-10-28, cairn/changes/gtd-and-time-tracking: `import --journal` and the VJOURNAL file are gone, the time entries going to the account's `time.strategy`; the M2 box above names what was built then. That change's MODIFIED requirements are applied to delta.md here, still to fold.
