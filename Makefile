run: persistent-bug.cabal
	cabal run
.PHONY: run

ghcid: persistent-bug.cabal
	ghcid
.PHONY: ghcid

persistent-bug.cabal:
	hpack
