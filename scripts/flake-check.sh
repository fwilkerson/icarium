#!/bin/sh
# Repeat the test suite to expose flakes, stopping at the first failure.
#
# Exists because a bare `for` loop can never be allow-listed: Claude Code's
# Bash permission matcher splits on &&, ||, ;, | and prefix-matches each part,
# but loops and $(...) do not decompose, so they are denied outright. Without
# this, a dispatched agent hunting a flake retypes N runs by hand and burns its
# budget doing it — which is how dispatch 01KXYMH313 lost its last 15 minutes.
#
#   scripts/flake-check.sh [RUNS] [tasty args...]
#   scripts/flake-check.sh 15 -p '/racing processes/'
set -e
runs="${1:-10}"
[ $# -gt 0 ] && shift

i=1
while [ "$i" -le "$runs" ]; do
    if ! out="$(cabal run -v0 icarium-test -- --hide-successes "$@" 2>&1)"; then
        printf 'flake-check: run %d/%d FAILED\n%s\n' "$i" "$runs" "$out" >&2
        exit 1
    fi
    printf 'flake-check: run %d/%d ok\n' "$i" "$runs"
    i=$((i + 1))
done
printf 'flake-check: %d runs clean\n' "$runs"
