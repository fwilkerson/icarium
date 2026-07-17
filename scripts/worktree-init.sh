#!/bin/sh
# Dispatch worktree provisioning. bin/ is gitignored ("reproduced via make
# install"), but make install only rebuilds icarium — the pre-commit hook
# needs the pinned fourmolu too, so copy it from the primary checkout.
# Failing loudly beats every worker rediscovering a broken commit gate.
set -e
main_root="$(dirname "$(git rev-parse --git-common-dir)")"
if [ ! -x "$main_root/bin/fourmolu" ]; then
    echo "worktree-init: $main_root/bin/fourmolu missing; commit gate would fail" >&2
    exit 1
fi
mkdir -p bin
cp "$main_root/bin/fourmolu" bin/fourmolu
