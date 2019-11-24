# ⌚ Unfog

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

![image](https://user-images.githubusercontent.com/10437171/69493746-c88ef100-0eb2-11ea-9dc2-c17dc7b4b8e4.png)

*Note: normal contexts (+tags) and special contexts can be used together.*
*Note: giving an empty (or invalid) context will clear it.*

### Worktime

Shows the total worktime spent on tasks belonging to the given context, grouped
by days. An empty context will show the worktime of all your tasks:


```bash
unfog wtime <+tag1> <+tag2> ...
```

![image](https://user-images.githubusercontent.com/10437171/69493775-2ae7f180-0eb3-11ea-88a3-a59eb088830e.png)

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
