# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Short ids for worktime --more query [#39]

## [1.0.1] - 2020-08-16

### Fixed

- List query as default command [#30]
- Completion [#37]
- Missing undelete command arg

## [1.0.0] - 2020-08-14

The args parser becomes more solid (based on
[optparse](https://github.com/pcapriotti/optparse-applicative#custom-parsing-and-error-handling)
lib). The due time is simplified and normalized (using ISO notation instead of
custom one), since it's not considered as a  main feature. Tags are replaced by
project. A project is just a unique tag (or a simple category). The aim of
those changes is to simplify the usage of Unfog, and to focus on the core
concept (create tasks, attach tasks to projects, start/stop tasks, check
reports).

### Added

- Save last state in a file [#25]

### Changed

- **[BREAKING]** Refactor id system [#33]
- **[BREAKING]** Refactor parser [#31]

### Fixed

- No parse due time [#34]

## [0.4.5] - 2020-07-27

### Changed

- Upgrade ghc `v8.8.4` to fix Windows build error

## [0.4.4] - 2020-07-27

### Fixed

- Bad build state for update cmd [#27]

### Added

- AUR package support [#26]
- Default command to list [#30]

### Changed

- Replace `Data.Duration` by own duration with tests [#24]
- Upgrade ghc `v8.8.3`

## [0.4.3] - 2020-03-11

### Added

- Init basic bash completion (commands, ids and tags) [#18]

## [0.4.2] - 2020-03-07

### Changed

- Remove secs and millis from queries [#23]
- All screenshots

### Fixed

- Bad wtime ordering [#22]
- Make install script more compatible by replacing `[[]]` with `[]`

## [0.4.1] - 2020-02-23

### Added

- `--more` option for worktime query

### Fixed

- `--json` output for worktime command

## [0.4.0] - 2020-01-11

### Added

- Upgrade command [#10]
- Add `status` command for UI integration
- Add `undone` command to unmark as done tasks [#20]

### Changed

- Shorter aliases
- Show command (UI) [#17]
- `worktime` command (group by ids) [#11]
- **[BREAKING]** Commands names + aliases

## [0.3.3] - 2020-01-01

### Added

- Colors [#4]

## [0.3.2] - 2019-12-30

### Added

- Refactor parsec exprs with `(<++)`
- Allow for batch processing of tasks [#13]

## [0.3.1] - 2019-12-28

### Fixed

- Active approx label (from `in ...` to `... ago`)

## [0.3.0] - 2019-12-28

### Added

- Due time [#9]

### Changed

- Make `+` optional in `context` and `worktime` commands [#16]

### Fixed

- Remove `print` [#15]

## [0.2.2] - 2019-12-22

### Added

- Worktime date range [#9]
- Due time [#9]

### Fixed

- Hyphen issue in desc [#12]

## [0.2.1] - 2019-12-08

### Added

- Add basic help command [#3]
- Add version command

### Changed

- Replace active ✔ by the elapsed time [#8]

### Fixed

- Delete command [#1]
- Worktime doesn't take in consideration the context [#2]

## [0.2.0] - 2019-12-01

### Added 

- Changelog
- New syntax to remove tags: `-tag` [#6]

### Changed

- **[BREAKING]** New parser introduced, based on parser combinators ([ReadP](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html)). Events have been impacted (simplified), so the previous store is not compatible anymore. You can upgrade it with this command: `sed -i -E 's/True |False |\+//g' ~/.config/unfog/store`

### Fixed 

- Create config dir if not exists [#5]

## [0.1.4] - 2019-11-26

### Added

- Installation script

### Changed

- Binaries compression (tar.gz)

## [0.1.3] - 2019-11-26

### Changed

- Build optimization

## [0.1.2] - 2019-11-26

### Added

- Travis CI
- Cross-compilation to Linux, OSX and Windows

## [0.1.1] - 2019-11-24

### Changed

- Update README and LICENSE

## [0.1.0] - 2019-11-23

First release :tada:

### Added

- Linux binaries

[unreleased]: https://github.com/soywod/unfog/compare/v1.0.1...HEAD
[1.0.1]: https://github.com/soywod/unfog/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/soywod/unfog/compare/v0.4.5...v1.0.0
[0.4.5]: https://github.com/soywod/unfog/compare/v0.4.4...v0.4.5
[0.4.4]: https://github.com/soywod/unfog/compare/v0.4.3...v0.4.4
[0.4.3]: https://github.com/soywod/unfog/compare/v0.4.2...v0.4.3
[0.4.2]: https://github.com/soywod/unfog/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/soywod/unfog/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/soywod/unfog/compare/v0.3.3...v0.4.0
[0.3.3]: https://github.com/soywod/unfog/compare/v0.3.2...v0.3.3
[0.3.2]: https://github.com/soywod/unfog/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/soywod/unfog/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/soywod/unfog/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/soywod/unfog/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/soywod/unfog/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/soywod/unfog/compare/v0.1.4...v0.2.0
[0.1.4]: https://github.com/soywod/unfog/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/soywod/unfog/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/soywod/unfog/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/soywod/unfog/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/soywod/unfog/releases/tag/v0.1.0

[#1]: https://github.com/soywod/unfog/issues/1
[#2]: https://github.com/soywod/unfog/issues/2
[#3]: https://github.com/soywod/unfog/issues/3
[#4]: https://github.com/soywod/unfog/issues/4
[#5]: https://github.com/soywod/unfog/issues/5
[#6]: https://github.com/soywod/unfog/issues/6
[#8]: https://github.com/soywod/unfog/issues/8
[#9]: https://github.com/soywod/unfog/issues/9
[#10]: https://github.com/soywod/unfog/issues/10
[#11]: https://github.com/soywod/unfog/issues/11
[#12]: https://github.com/soywod/unfog/issues/12
[#13]: https://github.com/soywod/unfog/issues/13
[#15]: https://github.com/soywod/unfog/issues/15
[#16]: https://github.com/soywod/unfog/issues/16
[#18]: https://github.com/soywod/unfog/issues/18
[#20]: https://github.com/soywod/unfog/issues/20
[#22]: https://github.com/soywod/unfog/issues/22
[#23]: https://github.com/soywod/unfog/issues/23
[#24]: https://github.com/soywod/unfog/issues/24
[#25]: https://github.com/soywod/unfog/issues/25
[#26]: https://github.com/soywod/unfog/issues/26
[#27]: https://github.com/soywod/unfog/issues/27
[#30]: https://github.com/soywod/unfog/issues/30
[#31]: https://github.com/soywod/unfog/issues/31
[#33]: https://github.com/soywod/unfog/issues/33
[#34]: https://github.com/soywod/unfog/issues/34
[#37]: https://github.com/soywod/unfog/issues/37
[#39]: https://github.com/soywod/unfog/issues/39
