{
  description = "fida-contracts";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://public-plutonomicon.cachix.org"
      "https://cache.sc.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc="
      "cache.sc.iog.io:b4YIcBabCEVKrLQgGW8Fylz4W8IvvfzRc+hy0idqrWU="
    ];
  };

  inputs = {
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/b7e8d396711f95e7a7b755a2a7e7089df712aaf5";
    haskell-nix.url = "github:input-output-hk/haskell.nix/9af167fb4343539ca99465057262f289b44f55da";
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    iohk-nix.follows = "cardano-transaction-lib/plutip/iohk-nix";
    CHaP.follows = "cardano-transaction-lib/plutip/CHaP";
    plutip.follows = "cardano-transaction-lib/plutip";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, CHaP, plutip, ...}:
  let
    nixpkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [
          (import "${iohk-nix}/overlays/crypto")
          haskell-nix.overlay
        ];
        inherit (haskell-nix) config;
      };

    hsProjectFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.haskell-nix.cabalProject {
          src = ./.;
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
          };
          compiler-nix-name = "ghc8107";
          modules = plutip.haskellModules;
          shell = {
            exactDeps = true;
            nativeBuildInputs = with pkgs; [
              # Shell utils
              bashInteractive
              git
              cabal-install
              ghcid

              # Lint / Format
              hlint
              haskellPackages.cabal-fmt
              haskellPackages.fourmolu
              nixpkgs-fmt

              haskellPackages.hasktags
            ];
            tools.haskell-language-server = { };
          };
        };

    formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = self.devShells.${system}.hs.nativeBuildInputs;
          } ''

          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'

          make format-check lint

          mkdir $out
        '';

    system = "x86_64-linux";

    hlswFor = system:
      let
        pkgs = nixpkgsFor system;
      in
      pkgs.writeShellApplication {
        name = "haskell-language-server-wrapper";

        runtimeInputs = self.devShells.${system}.hs.nativeBuildInputs;

        text = ''
          haskell-language-server "$@"
        '';
    };

  in
  {

    flake.${system} = (hsProjectFor system).flake { };

    packages.${system} = self.flake.${system}.packages;

    check.${system} =
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
                             ++ builtins.attrValues self.flake.${system}.packages
                             ++ self.devShells.${system}.hs.nativeBuildInputs;
          } "touch $out";

    checks.${system} = self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
    };

    devShells.${system} = rec {
        hs = self.flake.${system}.devShell;
        hlsw = hlswFor system;
        pkgs = nixpkgsFor system;
        default = (nixpkgsFor system).mkShell {
          inputsFrom = [ hs ];
          shellHook = ''
            export PATH=${hlsw}/bin:$PATH
            ${hs.shellHook}
          '';
        };
    };

  };
}
