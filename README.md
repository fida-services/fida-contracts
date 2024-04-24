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
