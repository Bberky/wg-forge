# wg-forge

[![CI](https://github.com/Bberky/wg-forge/actions/workflows/ci.yml/badge.svg)](https://github.com/Bberky/wg-forge/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/Bberky/wg-forge/branch/master/graph/badge.svg)](https://codecov.io/gh/Bberky/wg-forge)

wg-forge is a small CLI utility aiming to solve the complexity of managing multiple [WireGuard®](https://www.wireguard.com/) configurations. It allows you to declare your network configuration using a single YAML specification, from which per-node configuration files are generated. You can also generate QR codes for mobile devices and keep track of changes in your network using the built-in diff tool.

Instead of hand-editing one `wg-quick` config per peer and keeping their `[Peer]` blocks, addresses, and keys in sync by hand, you describe the network once — its peers and how they connect — and wg-forge compiles it into a complete, consistent set of `.conf` files.

## Features

- **Declarative YAML spec** — describe the whole network (peers + topology) in one file.
- **Strict validation** — the `validate` command accumulates and reports all errors at once, before anything is written.
- **Deterministic IP allocation** — addresses are assigned from the network CIDR reproducibly; the same spec always yields the same addresses. You can also pin static addresses per peer.
- **Automatic key management** — Curve25519 private keys are generated and stored on first `generate`, then reused.
- **Multiple topologies** — `full-mesh`, `hub-and-spoke`, and `relay` segments, freely combined in one network.
- **Diff against disk** — see exactly how your spec differs from the configs already deployed.
- **QR codes** — render any peer config to the terminal or to a PNG for easy mobile import.
- **Pure & self-contained** — no shelling out to `wg`, `wg-quick`, or `qrencode`; output is plain text you can inspect and commit.

## Installation

**Prerequisites:** [Stack](https://docs.haskellstack.org/) (it will fetch the pinned GHC 9.10 toolchain automatically).

Clone the repository and install the binary to your Stack bin path (`~/.local/bin` by default — make sure it is on your `PATH`):

```bash
git clone https://github.com/Bberky/wg-forge.git
cd wg-forge
stack install
```

This builds and installs the `wg-forge` executable. Verify it:

```bash
wg-forge --version
```

> wg-forge only *generates* WireGuard configuration files. To bring the resulting tunnels up you still need WireGuard itself (`wg` / `wg-quick`) installed on each node — see the [WireGuard installation guide](https://www.wireguard.com/install/).

If you prefer not to install globally, you can run any command through Stack from the repo with `stack run -- <command> ...`.

## Quick start

```bash
# 1. Scaffold a new project (creates the directory and a starter network.yaml)
wg-forge init --path my-net
cd my-net

# 2. Edit network.yaml to describe your peers and topology
$EDITOR network.yaml

# 3. Check the spec is valid (reports all errors at once)
wg-forge validate network.yaml

# 4. Generate per-peer configs (writes ./out/*.conf and stores keys in ./keys)
wg-forge generate network.yaml

# 5. Print a QR code for a mobile peer, or save it as a PNG
wg-forge qr out/node-a.conf
wg-forge qr --output node-a.png out/node-a.conf
```

Each peer in the spec gets its own `out/<peer>.conf`, ready to drop into `wg-quick`.

## CLI reference

The spec / config file is always a **positional** argument.

| Command | Synopsis | Description |
| --- | --- | --- |
| `init` | `wg-forge init --path DIR [--force]` | Scaffold a new project directory with a starter `network.yaml`. |
| `validate` | `wg-forge validate FILE` | Parse and validate a spec; report every error found. |
| `generate` | `wg-forge generate [--out DIR] [--keys DIR] FILE` | Compile the spec into one `.conf` per peer (generating keys as needed). |
| `diff` | `wg-forge diff [--out DIR] [--quiet] FILE` | Show how the spec differs from the configs already on disk. |
| `qr` | `wg-forge qr [--output FILE] FILE` | Encode a peer config as a QR code (terminal or PNG). |

Global flags: `-V` / `--version`, `--help`. Running `wg-forge` with no command prints the full help.

### Options

| Command | Option | Default | Notes |
| --- | --- | --- | --- |
| `init` | `-p`, `--path DIR` | *(required)* | Directory to create the project in. |
| `init` | `-f`, `--force` | off | Initialize even if the directory is not empty. |
| `generate` | `-o`, `--out DIR` | `out` | Output directory, resolved **relative to the spec file** unless absolute. |
| `generate` | `-k`, `--keys DIR` | `keys` | Private-key store, resolved **relative to the spec file** unless absolute. Missing keys are created. |
| `diff` | `-o`, `--out DIR` | `out` | Directory of on-disk configs to compare against. |
| `diff` | `-q`, `--quiet` | off | Suppress output for unchanged files. |
| `qr` | `-o`, `--output FILE` | *(terminal)* | Write a PNG to `FILE`. Without it, the QR code is printed to the terminal. |

### Exit codes

| Code | Meaning |
| --- | --- |
| `0` | Success |
| `1` | Usage error (bad arguments) |
| `2` | Spec validation error |
| `3` | I/O, keystore, or QR error |
| `4` | Diff found differences (only from `diff`) |

The `diff` exit code makes it easy to gate deploys in CI — a non-zero `4` means the on-disk configs have drifted from the spec.

## Network specification

A spec is a single YAML document with three top-level sections: `network` (global settings), `peers` (the nodes), and `segments` (how they connect). Unknown fields are rejected, so typos surface immediately.

```yaml
network:            # global settings
  name: my-network  # optional, for your reference
  cidr: 10.0.0.0/24 # required: address pool for automatic IP allocation

peers:              # the nodes in the network, keyed by a unique name
  node-a:
    endpoint: a.example.com:51820  # public host:port (omit for NAT-only peers)
    listenPort: 51820              # UDP port this peer listens on
    address: 10.0.0.1              # optional static IP (otherwise auto-allocated)
    persistentKeepalive: 25        # optional, seconds; useful behind NAT
    tags: [gateway]                # optional, free-form labels

segments:           # how peers connect (see Topologies below)
  mesh:
    topology: full-mesh
    peers: [node-a, node-b]
```

### Peer fields

All peer fields are optional — a peer can even be empty (`{}`) if it only appears in segments and gets an auto-allocated address.

| Field | Type | Description |
| --- | --- | --- |
| `endpoint` | `host:port` | Public endpoint others dial. Omit for peers behind NAT (they connect outbound only). |
| `listenPort` | port (0–65535) | UDP port this peer listens on. |
| `persistentKeepalive` | seconds | Keepalive interval; keeps NAT mappings alive for outbound-only peers. |
| `address` | IPv4 | Pin a static tunnel address. If omitted, one is allocated deterministically from `network.cidr`. |
| `tags` | list of strings | Free-form labels for your own documentation/filtering. |

### Topologies

Each segment declares a `topology`. Segments are additive — a peer can take part in several at once.

**`full-mesh`** — every peer connects directly to every other peer:

```yaml
segments:
  mesh:
    topology: full-mesh
    peers: [node-a, node-b, node-c]
```

**`hub-and-spoke`** — spokes connect only through the hub(s); spokes do not reach each other directly:

```yaml
segments:
  office:
    topology: hub-and-spoke
    hubs: [gateway]
    spokes: [laptop-alice, laptop-bob]
    allowedIps: subnet   # optional, default: peers
```

**`relay`** — relays forward traffic so clients can reach one another *through* the relay:

```yaml
segments:
  roaming:
    topology: relay
    relays: [relay-server]
    clients: [phone-carol, phone-dave]
    allowedIps: peers    # optional, default: peers
```

### `allowedIps` modes

For `hub-and-spoke` and `relay` segments, `allowedIps` controls what routes are placed in the generated `AllowedIPs`:

| Value | Effect |
| --- | --- |
| `peers` *(default)* | Only the `/32` host addresses of the segment's peers. |
| `subnet` | The entire `network.cidr` — route the whole subnet through this segment. |
| `internet` | A default route (`0.0.0.0/0`) — tunnel all traffic. |

### Worked examples

Two ready-to-run specs live in [`examples/`](examples/):

- [`examples/simple-mesh.yaml`](examples/simple-mesh.yaml) — a three-node full mesh.
- [`examples/complex-network.yaml`](examples/complex-network.yaml) — a network combining mesh, hub-and-spoke, and relay segments.

Try one:

```bash
wg-forge validate examples/simple-mesh.yaml
wg-forge generate examples/simple-mesh.yaml
```

## Generated output

`generate` writes one `wg-quick`-compatible `.conf` per peer. Each file has the peer's own `[Interface]` block followed by one `[Peer]` block for every node it can reach (determined by the segments it belongs to). For example, `out/node-a.conf` from [`examples/simple-mesh.yaml`](examples/simple-mesh.yaml) (keys shown are illustrative — yours are generated locally):

```ini
# Generated by wg-forge - do not edit.
# Source of truth: the network spec.

[Interface]
# node-a
PrivateKey = 6PqKSVvpIHZsDajr74Lh36kRutzafoldcshpSz3Z62s=
Address = 172.16.0.1
ListenPort = 51820

[Peer]
# node-b
PublicKey = PaS9r1vBN+pCPv2enXK58JPI/TPSjFykjm9NnsJdGBo=
Endpoint = a.example.com:51820
AllowedIPs = 172.16.0.2/32

[Peer]
# node-c
PublicKey = gamVNLjRIIvlyXgfsGyxuuAA+OZvq9tn33nv0gMpZWE=
Endpoint = a.example.com:51820
AllowedIPs = 172.16.0.3/32
```

Because output is deterministic, re-running `generate` on an unchanged spec rewrites nothing, and `wg-forge diff` will report a clean tree.

## Development

```bash
stack build      # build the library + executable
stack test       # run the hspec + QuickCheck suite
stack haddock    # generate API documentation
```

Code quality is enforced in CI:

```bash
fourmolu --mode check src/ app/ test/   # formatting
hlint src/ app/ test/                    # linting
```

CI additionally builds and tests with `--pedantic --coverage` on every push. All GHC warnings are enabled and the tree must compile warning-free.

## License

Released under the [MIT License](LICENSE).
