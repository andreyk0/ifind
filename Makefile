
build:
	cabal build

clean:
	cabal clean

deps:
	cabal install --dependencies-only

init:
	cabal sandbox init
	cabal install --dependencies-only
