#!/usr/bin/env bash
set -euo pipefail

# Must be run from the repository root
REPO_ROOT="$(git rev-parse --show-toplevel)"
if [[ "$REPO_ROOT" != "$PWD" ]]; then
    echo "error: run this script from the repository root"
    echo "  expected: $REPO_ROOT"
    echo "  current:  $PWD"
    exit 1
fi

# shellcheck source=tools/versions.env
source tools/versions.env

# Point git at the versioned hooks directory
git config core.hooksPath .githooks
chmod +x .githooks/pre-commit .githooks/commit-msg

echo "Git hooks configured (core.hooksPath = .githooks)"
echo ""

check_and_install() {
    local tool="$1"
    local want="$2"
    local pkg="$3"

    local status="not found"
    if command -v "$tool" &>/dev/null; then
        local got
        got="$("$tool" --version 2>&1 | grep -oE '[0-9]+\.[0-9]+(\.[0-9]+)*' | head -1)"
        if [[ "$got" == "$want" ]]; then
            printf "  %-10s %s (ok)\n" "$tool" "$want"
            return
        fi
        status="$got installed, want $want"
    fi

    printf "  %-10s %s — installing\n" "$tool" "$want  ($status)"
    stack install "${pkg}-${want}"
    printf "  %-10s %s (installed)\n" "$tool" "$want"
}

echo "Tool versions:"
check_and_install fourmolu "$FOURMOLU_VERSION" fourmolu
check_and_install hlint    "$HLINT_VERSION"    hlint
echo ""
echo "Done. Hooks are active for this clone."
