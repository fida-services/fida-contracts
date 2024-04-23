.PHONY: nix-cabal-repl format-cabal format-cabal-check format-hs-check format-hs hoogle lint tags requires-nix-shell format-nix-fmt format-nix-check ghcid-fida-contracts-serialise

CABAL_SOURCES := $(shell find . -type f -name "*.cabal")

NIX_SOURCES := $(shell find . type f -name "*.nix")

hoogle:
	@ nix develop -c hoogle server --local --port 8008

format-cabal: requires-nix-shell
	cabal-fmt --inplace $(CABAL_SOURCES)

format-cabal-check: requires-nix-shell
	cabal-fmt --check $(CABAL_SOURCES)

format-hs: requires-nix-shell
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence \
		$(shell find . -type f -name "*.hs" -o -name "*.lhs")

format-hs-check: requires-nix-shell
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence \
		$(shell find . -type f -name "*.hs" -o -name "*.lhs")

lint: requires-nix-shell
	hlint --no-summary $(shell find . -type f -name "*.hs" -o -name "*.lhs")

nix-cabal-repl:
	@ nix develop -c cabal new-repl

tags:
	hasktags -e -L . dist-newstyle/src

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

ghcid-fida-contracts-serialise:
	ghcid --command 'cabal repl fida-contracts-serialise'
