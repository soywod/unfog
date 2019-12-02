# ⌚ Unfog.cli [![Build Status](https://travis-ci.org/unfog-io/unfog-cli.svg?branch=master)](https://travis-ci.org/unfog-io/unfog-cli)

A simple task and time manager, written in [Haskell](https://www.haskell.org).

![image](https://user-images.githubusercontent.com/10437171/69493813-cd07d980-0eb3-11ea-911d-8d3e3f493b70.png)

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
    * [Remove](#remove)
    * [Context](#context)
    * [Worktime](#worktime)
  * [Options](#options)
    * [JSON](#json)
  * [Contributing](#contributing)
  * [Changelog](https://github.com/unfog-io/unfog-cli/blob/master/CHANGELOG.md#changelog)
  * [Credits](#credits)

## Installation
### From binaries

```bash
curl -sSL https://raw.githubusercontent.com/unfog-io/unfog-cli/master/install.sh | sh
```

*Note: Linux, OSX and Windows are supported. See the [releases
section](https://github.com/unfog-io/unfog-cli/releases).*


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

![image](https://user-images.githubusercontent.com/10437171/69493623-1145aa80-0eb1-11ea-8e34-f14bb3c831bc.png)

*Note: a tag should always start by `+`.*

### Show

```bash
unfog show <id>
```

![image](https://user-images.githubusercontent.com/10437171/69493650-6ed9f700-0eb1-11ea-99f7-77bee937ec3c.png)

### List

```bash
unfog list
```

![image](https://user-images.githubusercontent.com/10437171/69493551-338af880-0eb0-11ea-90fa-5c3c2f7783d4.png)

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

![image](https://user-images.githubusercontent.com/10437171/69493665-b2346580-0eb1-11ea-8cd4-46f3df331f5a.png)

### Done

Mark as done a task will remove it from the main list by adding a special tag `done`:

```bash
unfog done <id>
```

*Note: done tasks can be listed by enabling the [`done` context](#context).*

### Delete

```bash
unfog delete <id>
```

### Remove

The remove command acts like a toggle. If the task is already done, then it
[deletes it](#delete), otherwise it [marks it as done](#done).

### Context

Filters tasks by the given tags. Once set up:

- You will see only tasks containing at least one tag of your context
- When you [create](#create) a task, all tags in your context will be assigned
  to it

```bash
unfog context <+tag1> <+tag2> ...
```

The special context `+done` allows you to see done tasks:

```bash
unfog context +done
```

![image](https://user-images.githubusercontent.com/10437171/69493746-c88ef100-0eb2-11ea-9dc2-c17dc7b4b8e4.png)

*Note: giving an empty (or invalid) context will clear it.*

### Worktime

Shows the total worktime spent on tasks belonging to the given context, grouped
by days. An empty context will show the worktime of all your tasks:


```bash
unfog wtime <+tag1> <+tag2> ...
```

![image](https://user-images.githubusercontent.com/10437171/69493775-2ae7f180-0eb3-11ea-88a3-a59eb088830e.png)

## Options
### JSON

By adding the `--json` option, the output will be printed in JSON format:

```
{
  "ok": 0 | 1,
  "data": String | Task | [Task] | Worktime
}
```

```
Task {
  id: Int
  ref: Int
  pos: Int
  desc: String
  tags: [String]
  active: 0 | 1
  done: 0 | 1
  wtime: WtimeRecord
}

Worktime {
  total: WtimeRecord,
  wtimes: [{
    date: String (day)
    wtime: WtimeRecord,
  }]
}

WtimeRecord {
  approx: String (worktime approximation)
  human: String (full worktime)
  micro: Int (worktime in micro seconds)
}
```

![image](https://user-images.githubusercontent.com/10437171/69493950-e14cd600-0eb5-11ea-9804-1095e6deb73e.png)

This is useful to create user interafaces. Here the list of current implementations:

  - [Unfog.vim](https://github.com/unfog-io/unfog-vim) for Vim/Neovim

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

## Credits

- [Kronos](https://github.com/soywod/kronos.vim), the unfog predecessor
- [Taskwarrior](https://taskwarrior.org), a task manager
- [Timewarrior](https://taskwarrior.org/docs/timewarrior), a time tracker
