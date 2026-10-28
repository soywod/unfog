---
cairn: log
change: gtd-and-time-tracking
landed: 2026-10-28
---

# unfog, a GTD task manager with pluggable time tracking

unfog 2.x carries GTD and time tracking, against rust-rewrite's earlier split.

- **GTD** (src/shared/task/gtd.rs, src/gtd/, src/scope.rs): the convention of pimdir GUIDE.md §17, `inbox`, `waiting` and `someday` as reserved `CATEGORIES` compared case-insensitively, open being any status but `COMPLETED` and `CANCELLED`. Top-level `capture` (text or stdin), `inbox`, `next [-C]`, `waiting`, `someday`, `clarify` (interactive, or `--delete|--someday|--done|--waiting|--next` with `--summary`, `-C`, `--due`, `--contact`, `--to`, `--to-account`) and `review`, across every configured account. A clarification edits only the three reserved values, gated on the version read; a move creates in the target and removes from the source once the create is accepted.
- **pimdir views** decide from the store's task summaries (`status`, `categories`), a staged update read from its body; the FTS index is not used (tasks.md Notes). The local time collection is no tasklist any more.
- **Time tracking** (src/time/): `start`, `stop`, `status`, `worktime`, `time migrate --to`; `time.strategy` `event`, `journal` or `local` (default: pimdir's `U+0001time` written as the owner through io-pimdir's `put_item`, else a vdir directory). One running entry across accounts; `worktime` reads every place an account keeps entries in.
- **Import**: the 1.x project stays a plain category; starts and stops become entries of the account's strategy, keyed so a second import skips them. `--journal` is gone.
- **Config**: `capture.tasklist`, `time.strategy`, `time.collection`, `time.directory`, global or per account; config.sample.toml documents them.
- io-pimdir is patched to the sibling checkout (`path = "../io-pimdir"`) until its tasks-by-categories change is pinned to a rev.

Tests: 129 pass (`cargo test`), each view on vdir and pimdir fixtures, a cased `Inbox`, a person's category kept, a refused create keeping the capture, a staged clarification on pimdir, each strategy round tripping, a start stopping the running entry across accounts, worktime across three strategies, a migration keeping the worktime, the pimdir local time collection written unqueued; clippy clean on the default features.

Spec: gtd and time added (spec/gtd.md, spec/time.md). The MODIFIED requirements of rust-rewrite (one component kind, the command tree, open in the task listing, configuration, the import and its time entries) are applied to that change's delta.md, which is not folded yet; its proposal's Out and "What 1.x users lose" now say unfog carries GTD and time tracking and only `context` goes.
