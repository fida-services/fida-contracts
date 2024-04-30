.PHONY: nix-cabal-repl format-cabal format-cabal-check format-hs-check format-hs hoogle lint tags requires-nix-shell format-nix-check ghcid-fida-contracts-serialise format

CABAL_SOURCES := $(shell find . -type f -name "*.cabal" | grep -v ./dist-newstyle/ | grep -v ./.deps/)

NIX_SOURCES := $(shell find . -type f -name "*.nix" | grep -v ./dist-newstyle/)

HS_SOURCES := $(shell find src app test -type f -name "*.hs" -o -type f -name "*.lhs")

hoogle:
	@ nix develop .#hoogle -c hoogle server --local --port 8008

format-cabal: requires-nix-shell
	cabal-fmt --inplace $(CABAL_SOURCES)

format-cabal-check: requires-nix-shell
	cabal-fmt --check $(CABAL_SOURCES)

format-hs: requires-nix-shell
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence \
		$(HS_SOURCES)

format-hs-check: requires-nix-shell
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence \
		$(HS_SOURCES)

lint: requires-nix-shell
	hlint --no-summary $(HS_SOURCES)

nix-cabal-repl:
	@ nix develop -c cabal new-repl

requires-nix-shell:
	@ [ "$(IN_NIX_SHELL)" ] || { \
	echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"; \
	echo "    run 'nix develop' first"; \
	false; \
	}

format-nix: requires-nix-shell
	nixpkgs-fmt $(NIX_SOURCES)

format-nix-check: requires-nix-shell
	nixpkgs-fmt --check $(NIX_SOURCES)

format-check: format-nix-check format-cabal-check format-hs-check

format: format-nix format-cabal format-hs

ghcid-fida-contracts-serialise: requires-nix-shell
	ghcid --command 'cabal repl fida-contracts-serialise'

tags: requires-nix-shell
	rm -rf .deps
	cabal get bytestring optparse-applicative base aeson serialise -d .deps
	hasktags .deps app src ./dist-newstyle/src

test: requires-nix-shell
	cabal run fida-contracts-test
