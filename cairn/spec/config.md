---
cairn: spec
capability: config
status: current
---

# Configuration

The TOML configuration and the account wizard (src/config.rs, src/account/, src/wizard/).

### Requirement: Configuration

The configuration SHALL be read from `$XDG_CONFIG_HOME/unfog/config.toml` (or the paths `-c/--config` or `UNFOG_CONFIG` name, deep-merged), as `[accounts.<name>]` blocks each carrying one block per backend (`vdir.home-dir`, `pimdir.root`, `caldav.*` as in calendula), `tasklist.default`, `task.composer`, `capture.tasklist`, `time.strategy`, `time.collection`, `time.directory`, `time.account` and the table options, each settable globally and per account. `unfog configure` SHALL run the account wizard, which SHALL also be offered when a command needs an account and none is configured.
