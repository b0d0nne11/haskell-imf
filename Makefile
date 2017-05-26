build:
	stack build --haddock --no-copy-bins

test:
	stack test

clean:
	stack clean

dev-tools:
	stack setup
	stack install ghc-mod
	stack install stylish-haskell
	stack install hscolour

.PHONY: build test clean dev-tools
