# ⏱ Unfog [![gh-actions](https://github.com/soywod/unfog/workflows/CI/badge.svg)](https://github.com/soywod/unfog/actions?query=workflow%3ACI)

Minimalist CLI task & time manager, written in [Haskell](https://www.haskell.org).

![image](https://user-images.githubusercontent.com/10437171/89771094-1199da80-db00-11ea-8e65-12da9ec4161a.png)

## Table of contents

* [Motivation](#motivation)
* [Concept](#concept)
* [Installation](#installation)
* [Configuration](#configuration)
* [Usage](#usage)
* [Interfaces](#interfaces)
* [License](https://github.com/soywod/unfog/blob/master/LICENSE)
* [Changelog](https://github.com/soywod/unfog/blob/master/CHANGELOG.md#changelog)
* [Credits](#credits)

## Motivation

Taskwarrior is a good and powerful CLI to track your tasks, but it requires
time to configure it. The amount of features is giant, which can lead to
confusion.  Plus, if you also want to track your time, you need to install the
Timewarrior plugin, which makes the configuration step even heavier.

In the other hand, watson tracks well your time but can't tracks your tasks.

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

## Usage

### Add

Add a new task:

```bash
unfog add DESC [-p|--project PROJECT] [-d|--due DATE]
```

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
unfog list [--due-in MINS] [-d|--done] [-D|--deleted]
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

### Undelete

Undelete all tasks matching the ids:

```bash
unfog undelete IDs...
```

### Context

Define a project as context:

```bash
unfog context [PROJECT]
```

![image](https://user-images.githubusercontent.com/10437171/89776452-35621e00-db0a-11ea-91a0-3061b763eb37.png)

### Worktime

Show the total amount of time spent on tasks belonging to the given project,
grouped by day:

```bash
unfog worktime [PROJECT] [-f|--from DATE] [-t|--to DATE] [--more]
```

![image](https://user-images.githubusercontent.com/10437171/89777370-dc938500-db0b-11ea-9654-1be7195dd659.png)

The `--more` option will group tasks by day and by task in order to have a more
refined report:

![image](https://user-images.githubusercontent.com/10437171/89777413-ed43fb00-db0b-11ea-8f3a-c8111e3e749a.png)

### Upgrade

Upgrade the CLI with the latest release from
[GitHub](https://github.com/soywod/unfog/releases):

```bash
unfog upgrade
```

![image](https://user-images.githubusercontent.com/10437171/71656858-66d96680-2d3d-11ea-8ec9-1d9bb2b8712e.png)

## Interfaces

- [Unfog.vim](https://github.com/soywod/unfog.vim), a (neo)vim plugin

## Credits

- [Kronos](https://github.com/soywod/kronos.vim), the unfog predecessor
- [Taskwarrior](https://taskwarrior.org), a task manager
- [Timewarrior](https://taskwarrior.org/docs/timewarrior), a time tracker plugin for Taskwarrior
- [Watson](https://github.com/TailorDev/Watson), a time tracker
