.PHONY: build install test clean

build:
	cabal build all

install:
	cabal install --installdir=./bin --install-method=copy --overwrite-policy=always exe:icarium

test:
	cabal test all

clean:
	cabal clean
