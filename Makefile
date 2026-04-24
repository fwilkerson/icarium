.PHONY: build install test lint format clean

build:
	cabal build all

install:
	cabal install --installdir=./bin --install-method=copy --overwrite-policy=always exe:icarium

test:
	cabal test all

lint:
	hlint src/ app/

format:
	stylish-haskell -i src/**/*.hs app/**/*.hs

clean:
	cabal clean
