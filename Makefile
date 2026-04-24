.PHONY: build install test lint clean

build:
	cabal build all

install:
	cabal install --installdir=./bin --install-method=copy --overwrite-policy=always exe:icarium

test:
	cabal test all

lint:
	hlint src/ app/

clean:
	cabal clean
