# ⏱ Unfog

Minimalist CLI task & time manager, written in [Haskell](https://www.haskell.org).

*🚧 A rewrite in Rust is planned current 2025, stay tuned! 🚧*

![image](https://user-images.githubusercontent.com/10437171/89771094-1199da80-db00-11ea-8e65-12da9ec4161a.png)

## Unfog 2 (in development)

Unfog 2, rewritten in Rust (the `src/` crate), is a GTD task manager over plain iCalendar tasks (VTODO), wherever an account keeps them: a CalDAV server, a vdir folder, a pimdir store. It follows the convention the Pimalaya app shares, so a task captured on the phone is clarified in the terminal.

### What changes for 1.x users

`context` is gone: `gtd next --category <c>` replaces it. `start`, `stop` and `status` move under `task`, `worktime` stays, their time kept where `time.strategy` says. `unfog import <store>` reads a 1.x store: its projects become plain categories, its starts and stops time entries.

### GTD

A task's state is one of three reserved categories, `inbox`, `waiting` and `someday`, compared case-insensitively; every other category is your own (contexts, projects). A next action is an open task carrying none of the three, so a to-do another client wrote is one.

```sh
unfog gtd capture "Call Jo about the lease"   # or pipe a text, or a mail, to stdin
unfog gtd inbox
unfog gtd clarify                             # walk the Inbox, one task at a time
unfog gtd clarify 4c1e --next -C phone --due 2026-11-02 --priority 2
unfog gtd next -C phone
unfog gtd waiting
unfog gtd someday
unfog gtd review                              # counts, every view, who to chase
```

Next lists due tasks first, then by priority (`PRIORITY`, 1 the highest, set with `--priority` on `gtd clarify` or `task update`), then the oldest captured; the other views list the oldest captured first. Waiting shows who each task waits on and for how long, and `gtd review` ends with the tasks waiting over two weeks and, on pimdir, those whose mail got a reply. The views span every configured account (`-a` narrows them to one): an account that cannot be reached is warned about and the others still listed. `--json` prints each view as JSON, its schema given by `unfog json-schema`.

The short id a view prints tells apart every task of every account, so `task done`, `task update` and every other verb taking a task accept it, finding the task across every account and tasklist (`-a` and `-l` narrow the search). A mail piped to `gtd capture` gives its subject as the summary and a link back to it (`ATTACH:mid:`). `task update --category` replaces your own categories and keeps the GTD state; a task already in the state a clarification asks is left untouched. On a pimdir store a capture shows at once, before the next sync, and a move waits for the sync to push the task where it goes before removing it where it was.

### Time tracking

```sh
unfog task start 4c1e
unfog task status
unfog task stop
unfog worktime --since 2026-10-26 -C work
```

One entry runs at a time: a start stops the running one first. A running entry waits on the device, so `task status` sees it before any sync; `task stop` writes it finished where `time.strategy` says, per account:

| Strategy | Entry | Where | Synced |
| --- | --- | --- | --- |
| `local` (default) | a VEVENT | the pimdir store's local time collection on the pimdir backend, else `time.directory` (`$XDG_DATA_HOME/unfog/time`) | no |
| `event` | a VEVENT (`DTSTART`, `DTEND` once stopped, `TRANSP:TRANSPARENT`, `RELATED-TO` the task) | the calendar `time.collection` names, of the account `time.account` names when set | yes |
| `journal` | a pair of VJOURNALs, `CATEGORIES:start` and `stop` | the calendar `time.collection` names, of the account `time.account` names when set | where the server takes VJOURNAL |

Track time on more than one device with `event`. Give `time.collection` a calendar of its own that accepts VEVENT: only transparent events relating to a task count as entries, and any other event there is left alone. `worktime` reads every place an account keeps entries in, so a history survives a switch, and `unfog migrate --to event` moves the entries, keeping their UID, so a run cut short resumes. See config.sample.toml for every option.

## Unfog 1 (Haskell)

## Table of contents

* [Motivation](#motivation)
* [Concept](#concept)
* [Installation](#installation)
* [Configuration](#configuration)
* [Usage](#usage)
* [Interfaces](#interfaces)
* [License](https://github.com/soywod/unfog/blob/master/LICENSE)
* [Changelog](https://github.com/soywod/unfog/blob/master/CHANGELOG.md)
* [Credits](#credits)

## Motivation

[Taskwarrior](https://taskwarrior.org) is a good and powerful CLI to track your
tasks, but it requires time to configure it. The amount of features is giant,
which can lead to confusion.  Plus, if you also want to track your time, you
need to install the [Timewarrior](https://taskwarrior.org/docs/timewarrior)
plugin, which makes the configuration step even heavier.

In the other hand, [Watson](https://github.com/TailorDev/Watson) tracks well
your time but can't tracks your tasks.

Unfog proposes an alternative solution that:

- is easy to install / configure / use
- can track your tasks AND your time

## Concept

A task is composed of a description and an optionnal due time. It can be
attached to a project. It can be started and stopped (to track time) and done
or deleted (to track projects).

Basic reports can be generated to have an overview of your tasks and your
projects. They allow you to determine how and where the time was spent, if
projects fits their initial estimations…

## Installation

```bash
curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/install.sh | bash
```

*See [wiki section](https://github.com/soywod/unfog/wiki/Installation) for more
information.*

## Configuration

```toml
# ~/.config/unfog/config.toml

# Store file location.
# Default: $XDG_CONFIG_HOME/unfog/store
# Warning: only absolute paths are supported for now,
# see https://github.com/soywod/unfog/issues/45
store-path = "/abs/path/to/store"
```

*See [wiki section](https://github.com/soywod/unfog/wiki/Configuration) for
more information.*

## Usage

```
⏱ Unfog - Minimalist task & time manager

Usage: unfog COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  list                     Show current project tasks [l]
  info                     Show task details [i]
  worktime                 Show worktime report [wtime, w]
  status                   Show active task info [stat]
  add                      Add a new task [a]
  edit                     Edit an existing task [e]
  start                    Start a task [sta, s]
  stop                     Stop a task [sto, S]
  toggle                   Toggle a task [tog, t]
  done                     Mark as done a task [do, d]
  undone                   Unmark as done a task [undo, u]
  delete                   Delete a task [del, D]
  undelete                 Undelete a task [undel, U]
  context                  Change the current project [ctx, c]
  upgrade                  Upgrade the CLI
  version                  Show the version
  cache:clear              Clear the state cache
```

*See [wiki section](https://github.com/soywod/unfog/wiki/Usage) for more
information.*

## Interfaces

- [Unfog.vim](https://github.com/soywod/unfog.vim), a (neo)vim plugin

*See [wiki section](https://github.com/soywod/unfog/wiki/Interfaces) for more
information.*

# FAQ

## How can I get command history and completion?

While waiting for the Rust rewrite, you can use `rlwrap` to add these features:

```bash
#!/usr/bin/env bash

# Provides an interactive REPL (Read-Eval-Print Loop) for Unfog task manager.
# Features: command history, tab completion, and line editing. Commands are
# passed directly to unfog CLI.

set -euo pipefail

completion_file=$(mktemp)
trap 'rm -f "$completion_file"' EXIT

{
   unfog --bash-completion-index 0;
   printf 'help\n';
   printf 'quit\n';
} >> "$completion_file"

unfog_repl() {
    printf "Welcome to unfog REPL. Type 'quit' to exit.\n"
    local cmd
    while true; do
        read -r cmd || break

        [[ -z "$cmd" ]] && continue
        case "$cmd" in
             quit) break ;;
             help) unfog --help | sed -n '/Available commands:/,$p' ;;
             *) unfog $cmd ;;
         esac
    done
}

if ! command -v rlwrap >/dev/null 2>&1; then
    printf 'Error: rlwrap is not installed. Please install it first.'
    exit 1
fi

export -f unfog_repl

# run REPL
exec rlwrap \
    -p'Cyan' \
    -a \
    -H "${XDG_STATE_HOME:-$HOME/.local/state}/unfog_history" \
    -f "$completion_file" \
    -S 'unfog-repl> ' \
    bash -c unfog_repl
```

## Credits

- [Kronos](https://github.com/soywod/kronos.vim), the unfog predecessor
- [Taskwarrior](https://taskwarrior.org), a task manager
- [Timewarrior](https://taskwarrior.org/docs/timewarrior), a time tracker plugin for Taskwarrior
- [Watson](https://github.com/TailorDev/Watson), a time tracker

*See [wiki section](https://github.com/soywod/unfog/wiki/Credits) for more
information.*
