# ⏱ Unfog [![gh-actions](https://github.com/soywod/unfog/workflows/CI/badge.svg)](https://github.com/soywod/unfog/actions?query=workflow%3ACI)

Minimalist task & time manager, written in [Haskell](https://www.haskell.org).

![image](https://user-images.githubusercontent.com/10437171/89771094-1199da80-db00-11ea-8e65-12da9ec4161a.png)

## Table of contents

* [Concept](#concept)
* [Installation](#installation)
  * [From binaries](#from-binaries)
  * [From AUR](#from-aur)
  * [From sources](#from-sources)
  * [Completion](#completion)
* [Configuration](#configuration)
* [Usage](#usage)
  * [Add](#add)
  * [Info](#info)
  * [List](#list)
  * [Edit](#edit)
  * [Start](#start)
  * [Stop](#stop)
  * [Done](#done)
  * [Undone](#undone)
  * [Delete](#delete)
  * [Undelete](#undelete)
  * [Context](#context)
  * [Worktime](#worktime)
  * [Status](#status)
  * [Upgrade](#upgrade)
  * [Clear cache](#clear-cache)
* [Common options](#common-options)
  * [JSON](#json)
* [Contributing](#contributing)
* [Changelog](https://github.com/soywod/unfog/blob/master/CHANGELOG.md#changelog)
* [Credits](#credits)

## Concept

Unfog is a minimalist task & time manager. It helps you to track your projects.

A task is composed of a description, an optionnal due time and can be attached
to a project. Each task can be started, stopped, done or deleted. Basic reports
can be generated to have an overview of the situation.

Here a basic use case of a working day:

  - Set up the [context](#context) according to the current project
  - Have a look at [existing](#list) tasks
  - [Add](#add) new tasks if necessary
  - Pick one and [start](#start) it
  - [Stop](#stop) it once finished
  - Repeat the two last points during all the day
  - Check the [worktime](#worktime) report at the end of the day

At the end of the project, the worktime report shows useful
information about how long it took, how the time was spent, if it fit the
initial estimation, how it can be improved etc...

## Installation

### From binaries

```bash
curl -sSL https://raw.githubusercontent.com/soywod/unfog/master/bin/install.sh | bash
```

*Note: Linux, OSX and Windows are supported. See the [releases
section](https://github.com/soywod/unfog/releases).*


### From AUR

For Arch Linux users, there is an [AUR
package](https://aur.archlinux.org/packages/unfog-bin) available.

```bash
git clone https://aur.archlinux.org/unfog.git
cd unfog
makepkg -isc
```

### From sources

First you need to install
[stack](https://docs.haskellstack.org/en/stable/README/):

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Then build from git:
```bash
git clone https://github.com/soywod/unfog.git
cd unfog
stack install
```

### Completion

By default, only the bash completion is installed (in
`/etc/bash_completion.d/`).

If you are on [zsh](https://www.zsh.org/) or [fish](https://fishshell.com/),
you can get the completion file by running one of this command:

```bash
# For zsh users
unfog --zsh-completion-script `which unfog`

# For fish users
unfog --fish-completion-script `which unfog`
```

*Note: you will have to source manually those generated files.*

## Configuration

Unfog is customizable via a [TOML](https://github.com/toml-lang/toml) file
(`~/.config/unfog/config.toml`):

```toml
# Change the store path.
# Default: ~/.config/unfog/store
# Warning: only absolute paths are supported for now,
# see https://github.com/soywod/unfog/issues/45
store-path = "/abs/path/to/store"
```

## Usage

### Add

Add a new task:

```bash
unfog add DESC [-p|--project PROJECT] [-d|--due DATE]
```

- The description is a list of words
- The project can be a word
- The due date can be a date, following one of this format:
  - A full ISO datetime string `YYYY-MM-DD HH:MM`
  - A ISO date string `YYYY-MM-DD` (the time will be set at `00:00`)
  - A ISO time string `HH:MM` (the date will be set at `now`)

![image](https://user-images.githubusercontent.com/10437171/89775172-8de3ec00-db07-11ea-9843-b654953c5892.png)

### Info

Show more precise information about a specific task:

```bash
unfog info ID
```

![image](https://user-images.githubusercontent.com/10437171/89775322-de5b4980-db07-11ea-9250-2d9758bce905.png)

### List

Show all tasks belonging to the current context:

```bash
unfog list
```

![image](https://user-images.githubusercontent.com/10437171/89771094-1199da80-db00-11ea-8e65-12da9ec4161a.png)

### Edit

Edit a task:

```bash
unfog edit ID [DESC] [-p|--project PROJECT] [-d|--due DATE]
```

![image](https://user-images.githubusercontent.com/10437171/89775914-10b97680-db09-11ea-92f4-afd7726c8500.png)

### Start

Start all tasks matching the ids:

```bash
unfog start IDs...
```

### Stop

Stop all tasks matching the ids:

```bash
unfog stop IDs...
```

### Done

Do all tasks matching the ids:

```bash
unfog done IDs...
```

### Undone

Undo all tasks matching the ids:

```bash
unfog undone IDs...
```

### Delete

Delete all tasks matching the ids:

```bash
unfog delete IDs...
```

*Note: deleted tasks are excluded from any kind of report.*

### Undelete

Undelete all tasks matching the ids:

```bash
unfog undelete IDs...
```

### Context

```bash
unfog context [PROJECT]
```

Define a project as context. Once set up:

- The [list](#list) command will show only tasks belonging to this project
- When you [add](#add) a new task, it will automatically be attached to this
  project (except if you override it with the option `--project`)
- The [worktime](#worktime) will use this project by default if none is given

![image](https://user-images.githubusercontent.com/10437171/89776452-35621e00-db0a-11ea-91a0-3061b763eb37.png)

*Note: giving an empty context will clear it.*

### Worktime

```bash
unfog worktime [PROJECT] [-f|--from DATE] [-t|--to DATE] [--more]
```

Show the total amount of time spent on tasks belonging to the given project,
grouped by day. If no project given, the [context](#context) will be used.

You can also filter them with a date range (`--from` and `--to`). Date formats
are the same as [the due date format](#add).

![image](https://user-images.githubusercontent.com/10437171/89777370-dc938500-db0b-11ea-9654-1be7195dd659.png)

The `--more` option will group tasks by day and by task in order to have a more
refined report:

![image](https://user-images.githubusercontent.com/10437171/89777413-ed43fb00-db0b-11ea-8f3a-c8111e3e749a.png)

*Note: the `TOTAL WDAY` is the total in worktime days, where a worktime day is
define by default at 7.5 hours.*

### Status

Shows the status of the first active task found (desc + active duration):

```bash
unfog status
```

Useful for interface integration. Example with
[polybar](https://github.com/polybar/polybar):

```
[module/unfog]
type = custom/script
exec = unfog status
interval = 10
```

![image](https://user-images.githubusercontent.com/10437171/89777230-96d6bc80-db0b-11ea-8b57-f120059d561d.png)

### Upgrade

Upgrade the CLI with the latest release from
[GitHub](https://github.com/soywod/unfog/releases):

```bash
unfog upgrade
```

![image](https://user-images.githubusercontent.com/10437171/71656858-66d96680-2d3d-11ea-8ec9-1d9bb2b8712e.png)

### Clear cache

Clear the state cache (useful when the store is manually modified and does not
match the state anymore):

```bash
unfog cache:clear
```

## Common options

### JSON

By adding the `--json` option, the output will be printed in JSON format. This
is useful to create user interafaces. Here the list of current implementations:

  - [Unfog.vim](https://github.com/soywod/unfog.vim) for Vim/Neovim

To have more information about the JSON object shape, see
[Response.hs](https://github.com/soywod/unfog/blob/master/src/Response.hs).

## Contributing

Git commit messages follow the [Angular
Convention](https://gist.github.com/stephenparish/9941e89d80e2bc58a153), but
contain only a subject.

  > Use imperative, present tense: “change” not “changed” nor
  > “changes”<br>Don't capitalize first letter<br>No dot (.) at the end

Code should be as clean as possible, variables and functions use the camel case
convention. A line should be as short as possible to improve readability.

Tests should be added for each new functionality. Be sure to run tests before
proposing a pull request.

## Credits

- [Kronos](https://github.com/soywod/kronos.vim), the unfog predecessor
- [Taskwarrior](https://taskwarrior.org), a task manager
- [Timewarrior](https://taskwarrior.org/docs/timewarrior), a time tracker
- [Watson](https://github.com/TailorDev/Watson), a time tracker
