# ⌚ Unfog.cli

A simple task and time manager, written in [Haskell](https://www.haskell.org).

![image](https://user-images.githubusercontent.com/10437171/76142225-d6d0d980-606b-11ea-8253-044c1cb75824.png)

## Table of contents

* [Installation](#installation)
  * [From binaries](#from-binaries)
  * [From sources](#from-sources)
* [Usage](#usage)
  * [Add](#add)
  * [Info](#info)
  * [List](#list)
  * [Edit](#edit)
  * [Set](#set)
  * [Toggle](#toggle)
  * [Done](#done)
  * [Undone](#undone)
  * [Delete](#delete)
  * [Remove](#remove)
  * [Context](#context)
  * [Worktime](#worktime)
  * [Status](#status)
  * [Upgrade](#upgrade)
* [Options](#options)
  * [JSON](#json)
  * [More](#more)
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
### Add

To create a task, you need (no matter the order):

- A description
- A list of tags *(optional)*
- A due time *(optional)*

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
| `:010120` | 2020/01/01 00:00 |

```bash
unfog add|a desc (+tags) (:due:time)
```

![image](https://user-images.githubusercontent.com/10437171/76144755-aeed7000-6083-11ea-89df-799bba54c20b.png)

### Info

```bash
unfog info|i id
```

![image](https://user-images.githubusercontent.com/10437171/76144807-28855e00-6084-11ea-99b7-19b99ed9c470.png)

### List

```bash
unfog list|l
```

![image](https://user-images.githubusercontent.com/10437171/76142225-d6d0d980-606b-11ea-8253-044c1cb75824.png)

### Edit

Desc and due are replaced, but tags are kept. You can add one
with `+tag` and remove one with `-tag`.

```bash
unfog edit|e ids (desc) (+tags) (-tags) (:due:time)
```

### Set

Replace a task (nothing is kept, even the context).

```bash
unfog set|s ids desc (+tags) (:due:time)
```

### Toggle

Starts a task if stopped, or stops a task if started.

```bash
unfog toggle|t ids
unfog start|+ ids
unfog stop|- ids
```

### Done

Mark as done a task will remove it from the main list by adding a special tag
`done`:

```bash
unfog done|d ids
```

*Note: done tasks can be listed by enabling the [`done` context](#context).*

### Undone

Unmark as done a task will put back the task in the main list:

```bash
unfog undone|u ids
```

### Delete

```bash
unfog delete|D ids
```

### Remove

The remove command acts like a toggle. If the task is already done, then it
[deletes it](#delete), otherwise it [marks it as done](#done).

```bash
unfog remove|r ids
```

### Context

Filters tasks by the given tags. Once set up:

- You will see only tasks containing at least one tag of your context
- When you [create](#create) a task, all tags in your context will be assigned
  to it

```bash
unfog context|c (+tags)
```

The special context `+done` allows you to see done tasks:

```bash
unfog context +done
```

![image](https://user-images.githubusercontent.com/10437171/76144981-a302ad80-6085-11ea-8333-662b4d0e5a30.png)

*Note: the `+` is optional.*<br />
*Note: giving an empty (or invalid) context will clear it.*

### Worktime

Shows the total worktime spent on tasks belonging to the given context, grouped
by days. An empty context will show the worktime of all your tasks.

You can also filter them with a date range. Min date starts by `[`, and max
date by `]`. The date range should follow the due time format (see
[create](#create)).

```bash
unfog worktime|w (+tags) ([min:range) (]max:range) (--more)
```

![image](https://user-images.githubusercontent.com/10437171/76145256-540a4780-6088-11ea-988f-331299d77700.png)

The [`--more`](#more) will add (for each day) the tasks with their worktime:

![image](https://user-images.githubusercontent.com/10437171/76145348-fe826a80-6088-11ea-92a8-4c480c9e8007.png)

*Note: the `+` is optional.*<br>
*Note: the `TOTAL WDAY` is the total in worktime days, where a worktime day is
7.5 hours.*

### Status

Shows the status of the first active task found (desc + active duration).

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

### Upgrade

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
  wtime: TimeRecord
}

Worktime {
  total: TimeRecord,
  wtimes: [{
    date: String (day)
    wtime: TimeRecord,
  }]
}

TimeRecord {
  approx: String (worktime approximation)
  human: String (full worktime)
  micro: Int (worktime in micro seconds)
}
```

This is useful to create user interafaces. Here the list of current implementations:

- [Unfog.vim](https://github.com/unfog-io/unfog-vim) for Vim/Neovim

### More

By adding the `--more` option, you will get more informations depending on the
command:

- [Worktime](#worktime): adds for each day the tasks with their worktime

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
