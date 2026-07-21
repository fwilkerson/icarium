.PHONY: build install init test lint format watch clean

# -Werror lives here, not in the .cabal ghc-options: a release tarball or a
# future GHC that adds a new warning must still build. Local work must not.
# --enable-tests so test/ is compiled by a plain `make build` — without it
# cabal skips the test-suite entirely and its warnings never surface.
GHCFLAGS := --enable-tests --ghc-options=-Werror

build:
	cabal build all $(GHCFLAGS)

# One-time bootstrap on a fresh clone. Idempotent.
# Add future bootstrap steps here.
init:
	git config core.hooksPath .githooks
	@echo "git hooks installed (.githooks/pre-commit will run on commit)"

install:
	@actual=$$(git config --get core.hooksPath || true); \
	if [ "$$actual" != ".githooks" ]; then \
	    echo "warning: core.hooksPath is '$$actual' (expected .githooks); run 'make init'"; \
	fi
	cabal install --installdir=./bin --install-method=copy --overwrite-policy=always exe:icarium

test:
	cabal test all $(GHCFLAGS)

lint:
	hlint src/ app/ test/

format:
	find src app test -name '*.hs' | xargs ./bin/fourmolu --mode inplace

# Live recompile loop. Foreground; Ctrl-C to stop. Library only — faster
# reload than including the executable.
watch:
	./bin/ghcid --command="cabal repl lib:icarium" --warnings

clean:
	cabal clean
