# ⌚ Unfog

A simple task and time manager, written in [Haskell](https://www.haskell.org).

## Table of contents

  * [Installation](#installation)
    * [From binaries](#from-binaries)
    * [From sources](#from-sources)
  * [Usage](#usage)
    * [Create](#create)
    * [Show](#read)
    * [List](#list)
    * [Update](#update)
    * [Toggle](#toggle)
    * [Done](#done)
    * [Delete](#delete)
    * [Context](#context)
    * [Worktime](#worktime)
  * [Contributing](#contributing)
  * [Changelog](#changelog)
  * [Credits](#credits)

## Installation
### From binary

For now, only linux x86_64 is supported:

```
https://github.com/unfog-io/unfog-cli/releases/download/v0.1.0/unfog-linux-x86_64
```

### From sources

First you need to install
[stack](https://docs.haskellstack.org/en/stable/README/):

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Then build from git:
```bash
git clone https://github.com/unfog-io/unfog-cli.git unfog && cd unfog
stack install
```

## Usage
### Create

To create a task, you need:

  - A description
  - A list of tags *(optional)*

```bash
unfog create <desc> <+tag1> <+tag2> ...
```

*Note: a tag should always start by `+`.*

### Show

```bash
unfog show <id>
```

### List

```bash
unfog list
```

### Update

```bash
unfog update <id> <desc> <+tag1> <-tag2> ...
```

### Toggle

```bash
unfog start <id>
unfog stop <id>
unfog toggle <id>
```

### Done

```bash
unfog done <id>
```

*Note: done tasks can be listed with the [`done` context](#context).*

### Delete

```bash
unfog delete <id>
```

### Context

Filters tasks by the given tags. Once set up:

- You will see only tasks containing at least one tag of your context
- When you [create](#create) a task, all tags in your context will be assigned
  to it

```bash
unfog context <+tag1> <+tag2> ...
```

The special context `done` allows you to see done tasks:

```bash
unfog context done
```

*Note: normal contexts (+tags) and special contexts can be used together.*

### Worktime

Shows the total worktime spent on tasks belonging to the given context, grouped
by days. An empty context will show the worktime of all your tasks:

```bash
unfog wtime <+tag1> <+tag2> ...
```

## Contributing

Git commit messages follow the [Angular
Convention](https://gist.github.com/stephenparish/9941e89d80e2bc58a153), but
contain only a subject.

  > Use imperative, present tense: “change” not “changed” nor
  > “changes”<br>Don't capitalize first letter<br>No dot (.) at the end

Code should be as clean as possible, variables and functions use the camel case
convention. A line should never contain more than `80` characters.

Tests should be added for each new functionality. Be sure to run tests before
proposing a pull request.

## Changelog

- **Nov. 23, 2019** - First release v0.1.0

## Credits

- [Kronos](https://github.com/soywod/kronos.vim), the unfog predecessor
- [Taskwarrior](https://taskwarrior.org), a task manager
- [Timewarrior](https://taskwarrior.org/docs/timewarrior), a time tracker
  Taskwarrior wrapper for vim
