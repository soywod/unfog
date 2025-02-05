# ⏱ Unfog [![gh-actions](https://github.com/soywod/unfog/workflows/deployment/badge.svg)](https://github.com/soywod/unfog/actions?query=workflow%3Adeployment)

Minimalist CLI task & time manager, written in [Haskell](https://www.haskell.org).

![image](https://user-images.githubusercontent.com/10437171/89771094-1199da80-db00-11ea-8e65-12da9ec4161a.png)

*🚧 A rewrite in Rust is planned current 2025, stay tuned! 🚧*

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
