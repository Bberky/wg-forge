# wg-forge

`wg-forge` is a declarative WireGuard network manager written in Haskell. It enables users to define their WireGuard mesh network in a YAML specification, and then generates `wg-quick` configuration files and QR codes for each peer. This tool is designed to simplify the management of WireGuard networks.

## Motivation

Maintaining a WireGuard network manually can become error-prone as the number of peers increases. Each new node requires editing every existing peer's configuration, IP allocation can drift, and topology changes can be difficult to manage. Existing tools either emit configurations from a flat peer list ([wg-meshconf](https://github.com/k4yt3x/wg-meshconf)) or require full control-plane systems with their own coordination servers ([TailScale](https://tailscale.com/), [Headscale](https://headscale.net/stable/), [NetBird](https://netbird.io/), [Innernet](https://github.com/tonarino/innernet)), which may not be ideal for homelab operators who want to keep their network configurations in plain text and version-controlled.

`wg-forge` takes a **declarative** approach, where a typed network specification serves as the single source of truth. The tool derives every per-peer configuration from this specification in a deterministic and idempotent manner.

## Overview

`wg-forge` is a CLI tool that reads a YAML mesh specification and produces:

- `wg-quick`-compatible configuration files for every peer
- QR codes (terminal and PNG) for one-tap onboarding of mobile peers
- A structured diff between the spec and the existing generated output

The initial version supports **full-mesh** topology: every listed peer connects to every other listed peer.

## Functional Requirements

### F1. Declarative network specification

- **F1.1** Parse YAML network specifications into a typed AST.
- **F1.2** A spec defines: a network with a CIDR, and a list of peers (name, endpoint, listen port, NAT status, optional tags).
- **F1.3** Topology is a single full mesh between all listed peers.
- **F1.4** Validation accumulates **all** errors before failing (applicative validation), reporting: IP collisions, CIDR overflow, duplicate peer names, missing endpoints on non-NATed peers.

### F2. Configuration generation

- **F2.1** Compile a spec into `Map PeerName CompiledPeer`.
- **F2.2** Render each compiled peer as a `wg-quick`-compatible config (`<peer>.conf`).
- **F2.3** IP allocation within the CIDR is deterministic: the same spec always yields the same address assignments across runs and across machines.
- **F2.4** Output directory layout is reproducible and documented.
- **F2.5** Generation is **idempotent at the file level**: a file is only rewritten if its byte content would change. Files whose generated content is identical to the existing file are left untouched.
- **F2.6** Generation is **safe to re-run**: running `wgforge generate` twice in a row with no spec changes is a no-op.

### F3. Key management

- **F3.1** Generate WireGuard private keys per peer on demand.
- **F3.2** Store private keys as plain files in a local keystore directory.
- **F3.3** Public keys are derived from private keys at generation time and never stored separately.

### F4. QR code generation

- **F4.1** `wgforge qr <peer>` prints the peer's full `wg-quick` configuration as a QR code rendered in the terminal using Unicode block characters.
- **F4.2** `wgforge qr <peer> --out <file>.png` saves the QR code as a PNG image.
- **F4.3** The QR encodes the exact same content as the corresponding `<peer>.conf`, so it is directly scannable by the official WireGuard mobile apps.

### F5. Diff

- **F5.1** `wgforge diff` compares the would-be output of the current spec against the existing files in the output directory, without writing anything.
- **F5.2** The diff is structured per peer: each peer is reported as **added**, **removed**, **changed**, or **unchanged**.
- **F5.3** For changed peers, a line-level unified diff between the existing file and the would-be new content is shown.
- **F5.4** Exit code is `0` when there are no pending changes and `4` (a dedicated "diff dirty" code) when there are — suitable for use as a CI / pre-commit check that the committed configs are in sync with the spec.
- **F5.5** A `--quiet` flag suppresses the diff body and reports only the per-peer status summary.

### F6. Command-line interface

- **F6.1** Subcommands: `init`, `validate`, `generate`, `diff`, `qr`.
- **F6.2** Help text (`--help`) for the root command and every subcommand.
- **F6.3** Standardised exit codes: `0` success, `1` usage error, `2` spec validation error, `3` IO error, `4` diff dirty (only from `diff`).

## Non-Functional Requirements

### N1. Build and distribution

- **N1.1** Distributed as a Stack project with a pinned LTS resolver.
- **N1.2** A single binary buildable via `stack build`.
- **N1.3** `stack install` places a usable executable on the user's `PATH`.

### N2. Testing

- **N2.1** `hspec` for unit and integration tests.
- **N2.2** `QuickCheck` property tests on the pure cores: compilation, IP allocation, validation, diff.
- **N2.3** Idempotency property explicitly tested: for any valid spec, `compile spec == compile spec` byte-for-byte; running `generate` twice produces identical output and rewrites no files on the second run.
- **N2.4** Tests are executed by CI on every push and pull request.

### N3. Documentation

- **N3.1** Haddock comments on every exported identifier; documentation buildable via `stack haddock`.
- **N3.2** `README.md` covers installation, dependencies, basic usage, and a worked example.
- **N3.3** An `examples/` directory contains at least two sample specs with their expected outputs.

### N4. CI/CD

- **N4.1** GitHub Actions pipeline running on every push and pull request: build, test, `hlint`, formatting check (`fourmolu --mode check`), Haddock generation.
- **N4.2** Build cache configured for faster iteration.

### N5. Code quality

- **N5.1** All code, comments, documentation, and commit messages in English.
- **N5.2** Consistent formatting enforced by `fourmolu` (config committed to repo).
- **N5.3** Linting via `hlint` (config committed).
- **N5.4** Atomic commits — each commit addresses a single self-contained change with an explanatory message.

### N6. Platform support

- **N6.1** Support for Linux and macOS (the primary platforms for WireGuard users).

## Possible Extensions

This is a list of features that are out of scope for the initial version but could be added in the future:

- **Hub-and-spoke, relay, and link topologies** in addition to the full mesh.
- **Encrypted keystore** with passphrase-based unlocking.
- **`graph` subcommand** to emit Mermaid / Graphviz topology diagrams.
- **SSH-based deployment** of generated configs to remote hosts.

## References

- WireGuard — <https://www.wireguard.com/>
- `wg-quick(8)` config format
- `wg-meshconf` (Python prior art) — <https://github.com/k4yt3x/wg-meshconf>
