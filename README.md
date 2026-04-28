# icarium

Task/knowledge/dispatch tool for headless-agent workflows.

## Pre-1.0 note

<!-- TODO(post-v0.1) remove this section -->
Before v0.1.0 was tagged, the migration chain was squashed; existing development DBs from earlier checkouts must be re-initialized (`rm .icarium/icarium.db && ./bin/icarium init`).

## Development

### Build

```sh
make build   # cabal build all
make install # copy binary to ./bin/icarium
make test    # cabal test all
```

### Lint

```sh
make lint    # runs hlint src/ app/
```

Install HLint if not present:

```sh
# via cabal (one-time, slow first build due to ghc-lib-parser dep):
cabal install hlint --installdir=~/.local/bin

# or via brew (prebuilt, faster):
brew install hlint
```

HLint settings live in `.hlint.yaml` at the repo root.
