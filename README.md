fida-contracts
==============

### Generate purescript module

```
nix run -L git+ssh://git@github.com/fida-services/fida-contracts#fida-contracts:exe:fida-contracts-serialise -- -n 'Fida.RawScript'
```

locally
```
nix run -L .#fida-contracts:exe:fida-contracts-serialise -- -n 'Fida.RawScript'
```

### Generate plutus scripts

```
nix run -L git+ssh://git@github.com/fida-services/fida-contracts#fida-contracts:exe:fida-contracts-serialise -- -o .
```

locally
```
nix run -L .#fida-contracts:exe:fida-contracts-serialise -- -o ~/now/fida-purs-module/
```

### VS Code Setup

- Install direnv

```
nix profile install nixpkgs#direnv
```

and add following line

for zsh -> to the `~/.zshrc`

```
eval "$(direnv hook zsh)"
```

for bash -> `~/.bashrc`

```
eval "$(direnv hook bash)"
```

- Install https://github.com/nix-community/nix-direnv

```
nix profile install nixpkgs#nix-direnv
```

and add following to the `~/.zshrc` (`~/.bashrc`) file

```
source $HOME/.nix-profile/share/nix-direnv/direnvrc
```

- Allow .envrc to be loaded

```
cd fida-contracts
direnv allow
```

- Install VS Code extensions

* direnv
* Haskell