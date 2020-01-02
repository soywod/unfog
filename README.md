# ⌚ Unfog.cli

A simple task and time manager, written in [Haskell](https://www.haskell.org).

![image](https://user-images.githubusercontent.com/10437171/71645664-d1f25100-2cdb-11ea-9b84-620874793e69.png)

## Table of contents

* [Installation](#installation)
  * [From binaries](#from-binaries)
  * [From sources](#from-sources)
* [Usage](#usage)
  * [Create](#create)
  * [Show](#read)
  * [List](#list)
  * [Update](#update)
  * [Replace](#replace)
  * [Toggle](#toggle)
  * [Done](#done)
  * [Delete](#delete)
  * [Remove](#remove)
  * [Context](#context)
  * [Worktime](#worktime)
  * [Upgrade](#upgrade)
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
- A due time *(optional)*

```bash
unfog create desc (+tags) (:due:time) # alias: add
```

![image](https://user-images.githubusercontent.com/10437171/69493623-1145aa80-0eb1-11ea-8e34-f14bb3c831bc.png)

A tag should start by `+` and should be composed of alpha numeral chars.

A due time should follow the format `:DDMMYY:HHMM`, where almost everything is
optional. Each omitted field is replaced by the current date one. Here some
use cases (given now = `2019/12/22 10:00`):

| Given arg | Due time |
| --- | --- |
| `:21` | 2019/12/21 00:00 |
| `:18:8` | 2019/12/18 08:00 |
| `::12` | 2019/12/22 12:00 |
| `:1010` | 2019/10/10 00:00 |

### Show

```bash
unfog show id
```

![image](https://user-images.githubusercontent.com/10437171/69493650-6ed9f700-0eb1-11ea-99f7-77bee937ec3c.png)

### List

```bash
unfog list
```

![image](https://user-images.githubusercontent.com/10437171/69493551-338af880-0eb0-11ea-90fa-5c3c2f7783d4.png)

### Update

Update a task. Desc and due are replaced, but tags are kept. You can add one
with `+tag` and remove one with `-tag`.

```bash
unfog update ids (desc) (+tags) (-tags) (:due:time) # alias: edit
```

### Replace

Replace a task (nothing is kept).

```bash
unfog replace ids desc (+tags) (:due:time) # alias: set
```

### Toggle

Starts a task if stopped, or stops a task if started.

```bash
unfog start ids
unfog stop ids
unfog toggle ids
```

![image](https://user-images.githubusercontent.com/10437171/69493665-b2346580-0eb1-11ea-8cd4-46f3df331f5a.png)

### Done

Mark as done a task will remove it from the main list by adding a special tag `done`:

```bash
unfog done ids
```

*Note: done tasks can be listed by enabling the [`done` context](#context).*

### Delete

```bash
unfog delete ids
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
unfog context +tags # alias: ctx
```

The special context `+done` allows you to see done tasks:

```bash
unfog context +done
```

*Note: the `+` is optional.*

![image](https://user-images.githubusercontent.com/10437171/69493746-c88ef100-0eb2-11ea-9dc2-c17dc7b4b8e4.png)

*Note: giving an empty (or invalid) context will clear it.*

### Worktime

Shows the total worktime spent on tasks belonging to the given context, grouped
by days. An empty context will show the worktime of all your tasks.

You can also filter them with a date range. Min date starts by `[`, and max
date by `]`. The date range should follow the due time format (see [#create]).

```bash
unfog worktime +tags ([min:range) (]max:range) # alias: wtime
```

![image](https://user-images.githubusercontent.com/10437171/69493775-2ae7f180-0eb3-11ea-88a3-a59eb088830e.png)

*Note: the `+` is optional.*

### Upgrade

Upgrade the executable:

```bash
unfog upgrade
```

![image](https://user-images.githubusercontent.com/10437171/71656858-66d96680-2d3d-11ea-8ec9-1d9bb2b8712e.png)

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
  active: TimeRecord
  due: TimeRecord
  done: 0 | 1
  wtime: {
    total: TimeRecord,
    wtimes: [{
      date: String (day)
      wtime: TimeRecord,
    }]
  }
}

TimeRecord {
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
