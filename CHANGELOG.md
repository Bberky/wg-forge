# Changelog for `wg-forge`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- Network spec data structures (`NetworkSpec`, `PeerSpec`, `SegmentSpec`, endpoints)
- YAML spec parsing with `aeson`/`yaml`, including CIDR and endpoint parsing
- Spec validation with applicative error accumulation: topology integrity,
  endpoint rules, reachability, extra keys, and addressing
- Test suite (`hspec`) covering parsers and validators, with shared fixtures
- CI with build, test, formatting (`fourmolu`) and lint (`hlint`) checks
- Git hooks for conventional commits, formatting, and linting

### Changed

- License changed to MIT
