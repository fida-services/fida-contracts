.PHONY: nix-cabal-repl format-cabal format-cabal-check format-check format hoogle lint tags requires-nix-shell

CABAL_SOURCES := $(shell find . -type f -name "*.cabal")

hoogle:
	@ nix develop -c hoogle server --local --port 8008

format-cabal: requires-nix-shell
	cabal-fmt --inplace $(CABAL_SOURCES)

format-cabal-check: requires-nix-shell
	cabal-fmt --check $(CABAL_SOURCES)

format: requires-nix-shell
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence \
		$(shell fd -ehs -elhs)

format-check: requires-nix-shell
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence \
		$(shell fd -ehs -elhs)

lint: requires-nix-shell
	hlint --no-summary $(shell fd -ehs -elhs)

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
