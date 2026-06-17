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
- Deterministic IP allocation of peers within the network CIDR
- Keystore: Curve25519 key generation and create-only plain-file storage
  (`0600` keys in a `0700` directory, existing keys never regenerated)
- Pure compilation from a validated spec to per-peer configs
  (`Map PeerName CompiledPeer`): tunnels, allowed IPs, and addressing
- `wg-quick` config rendering with `prettyprinter` (stable, timestamp-free output)
- Command-line interface with `init`, `validate`, and `generate` subcommands
- `init` scaffolds a project (starter spec, `out/`, `0700 keys/`), refusing a
  non-empty directory without `--force`
- `generate` runs the full pipeline to per-peer `<peer>.conf` files plus keys,
  with idempotent atomic writes (re-runs rewrite nothing) and output resolved
  relative to the spec's directory
- Structured error reporting mapped to exit codes (usage `1`, spec/validation
  `2`, I/O and keystore `3`)
- Full help with the command list when invoked with no arguments
- Test suite (`hspec`) covering parsers, validators, allocation, keystore, and
  the CLI, with shared fixtures
- CI with build, test, formatting (`fourmolu`) and lint (`hlint`) checks
- Git hooks for conventional commits, formatting, and linting

### Changed

- License changed to MIT
