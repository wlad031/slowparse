# Changelog
All notable changes to this project will be documented in this file.

## [0.1.0] - 2021-02-24

Initial release.

## [0.1.3] - 2022-09-29

### Added

- `charsWhile`/`charsUntilIn` parsers.
- `s`/`s0`/`s1` parsers.
- `&` - positive lookahead parser.

## [0.1.4] - 2022-10-03

### Changed

- `until` parser now accepts a "collector" parser instead of always using `anyChar` internally (however, by default, `collector = anyChar`).  
  This allows to implement a parser which can parse JSON strings containing escaped quotes.

[0.1.0]: https://github.com/wlad031/slowparse/releases/tag/v0.1.0
[0.1.3]: https://github.com/wlad031/slowparse/releases/tag/v0.1.3
[0.1.4]: https://github.com/wlad031/slowparse/releases/tag/v0.1.4