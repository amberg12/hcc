.RECIPEPREFIX = >

.PHONY: all
all: build

.PHONY: build
build:
> cabal update
> cabal build

.PHONY: run
run:
> @cabal run hcc

.PHONY: format
format:
> git ls-files -z '*.hs' | xargs -P 1 -0 fourmolu --mode inplace