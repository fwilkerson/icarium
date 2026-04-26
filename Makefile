.PHONY: build install init test lint format clean

build:
	cabal build all

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
	cabal test all

lint:
	hlint src/ app/ test/

format:
	find src app test -name '*.hs' | xargs ./bin/stylish-haskell -i

clean:
	cabal clean
