fida-contracts
==============

On-chain contracts of the Fida insurance protocol: Plutus V2 scripts written in Scala 3 and compiled
to UPLC with [Scalus](https://scalus.org).

| Script                     | Kind           | Parameters                                     |
|----------------------------|----------------|------------------------------------------------|
| `InsuranceIdMintingPolicy` | minting policy | `TxOutRef` that the minting transaction spends |
| `InsurancePolicyValidator` | validator      | `InsuranceId`                                  |
| `PiggyBankValidator`       | validator      | `InsuranceId`, `FidaCardId`                    |
| `InvestorValidator`        | validator      | investor `PubKeyHash`, `SystemId`              |
| `SystemIdMintingPolicy`    | minting policy | governance `PubKeyHash`, magic `Integer`       |

### Prerequisites

- JDK 21 (the version CI uses)
- [sbt](https://www.scala-sbt.org/download/) 1.x: any launcher works, it fetches the sbt version
  pinned in `project/build.properties`

Or, with [Nix](https://nixos.org/download/) (flakes enabled), `nix develop` opens a shell with JDK 21,
sbt, coursier and Metals (the first run pins nixpkgs in a new `flake.lock`, to commit).

### Build and test

```
sbt compile
sbt test
sbt "testOnly fida.contract.DataEncodingSpec"
```

`make test` runs `sbt test`. Without a local sbt: `nix develop -c sbt test`.

### Generate plutus scripts

The serialise tool (`fida.serialise.Main`, the former `fida-contracts-serialise` executable) writes
each script, unapplied, to `<DIR>/<Name>.script` as a cardano-api text envelope
(`{"type": "PlutusScriptV2", "description": "", "cborHex": "…"}`):

```
sbt "run -o ."
sbt "run -o $HOME/now/fida-purs-module"
```

`make scripts` writes them to `out/scripts` (`make scripts SCRIPTS_DIR=…` to change it).

### Generate purescript module

`-n MODULE` emits a PureScript module exporting each script's text envelope as `raw<Name> :: String`,
for cardano-transaction-lib. It goes to standard output (`--error` keeps sbt's own messages out of
it), or to a file with `-p PATH`:

```
sbt --error "run -n Fida.RawScripts" > RawScripts.purs
sbt "run -n Fida.RawScripts -p ../fida-frontend/src/Fida/RawScripts.purs"
```

`make purs` writes `Fida.RawScripts` to `out/purs/Fida/RawScripts.purs` (`PURS_MODULE=…`,
`PURS_FILE=…` to change them).

Every CI run also uploads the scripts and the module as the `fida-contracts-scripts` workflow
artifact, so they can be downloaded without building.

### Formatting

Scala sources and sbt files are formatted with [scalafmt](https://scalameta.org/scalafmt/)
(`.scalafmt.conf`): `make format` (`sbt scalafmtAll scalafmtSbt`) and `make format-check`
(`sbt scalafmtCheckAll scalafmtSbtCheck`).

### VS Code Setup

- Install direnv

```
nix profile install nixpkgs#direnv
```

and add the following line

to the `~/.zshrc` (for `zsh` users)

```
eval "$(direnv hook zsh)"
```

or to `~/.bashrc` (for `bash` users)

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

- Allow `.envrc` (`use flake`) to be loaded, which puts the dev shell (JDK 21, sbt, Metals) and
  its `JAVA_HOME` in the environment

```
cd fida-contracts
direnv allow
```

- Open `fida-contracts.code-workspace` and install the recommended VS Code extensions
  * direnv (`mkhl.direnv`)
  * Scala (Metals) (`scalameta.metals`)

- When Metals asks, import the sbt build. Scala files are formatted on save with the project's
  scalafmt configuration.

Without Nix, install JDK 21 and sbt yourself; Metals finds the JDK through `JAVA_HOME`.

### Project layout

```
build.sbt, project/            sbt build: Scalus compiler plugin, scalafmt plugin
src/main/scala/fida/
  contract/
    Scripts.scala              compiled scripts, compiler options, parameter application
    Utils.scala                on-chain helpers
    SystemId.scala             SystemId minting policy
    Investor.scala             investor validator
    insurance/
      Datum.scala              datums and their on-chain encodings
      Redeemer.scala           redeemers
      Authority.scala          insurance authority signing strategies
      Tokens.scala             token names
      InsuranceId.scala        InsuranceId minting policy
      InsurancePolicy.scala    insurance policy validator: dispatch and expiry
      PiggyBank.scala          piggy bank validator
      lifecycle/               Initiated, Funding, OnRisk and Cancelled policy states
  serialise/Main.scala         the serialise tool (`sbt run`)
src/test/scala/fida/
  contract/                    contract tests: direct script evaluation and emulator tests
    testkit/                   script evaluation, script-context builders, sample values
  serialise/                   serialise tool tests
```

### Migration from Haskell

The contracts used to be written in Haskell with PlutusTx and built with Nix and haskell.nix. The
Scala port keeps them compatible with the existing off-chain code:

- **Plutus V2 kept.** The five scripts are still Plutus V2 scripts (`PlutusScriptV2` envelopes),
  with the same names, parameters and `ERR-…` error codes.
- **UPLC 1.0.0.** The scripts are compiled for protocol version 10 (Plomin), so they are plain UPLC
  1.0.0 programs (no `constr`/`case`) that existing off-chain tooling handles. They run unchanged on
  later protocol versions.
- **Same on-chain data.** Datum, redeemer and parameter encodings are unchanged: the same
  `makeIsDataIndexed` constructor indices, newtypes (`InsuranceId`, `FidaCardId`, `SystemId`,
  `POSIXTime`, …) encoded as the type they wrap, and the plutus-ledger-api V2 encodings of
  `TxOutRef`, `Address`, `Maybe` and `Bool`. `DataEncodingSpec` checks every type against the
  Haskell encoding. (`PiggyBankRedeemer` lists its cases in on-chain index order: the Haskell type
  declared `ClaimPremiumOnCancel` before `PayForClaimWithCollateral` but indexed them 4 and 3.)
- **New script hashes.** The scripts come from a different compiler (Scalus instead of PlutusTx and
  Plutonomy), so their bytes differ, and with them the script hashes: the `InsuranceId` and
  `SystemId` currency symbols and the validator addresses change. Outputs locked by the Haskell
  scripts can only be spent with those scripts.
- **Parameters applied off-chain, as before.** The tool serialises the scripts unapplied; parameters
  are applied at the UPLC level by wrapping the program in applications to `Data` constants, as
  cardano-transaction-lib's `applyArgs` does. The `Scripts.*Script` helpers do the same on the JVM.

| Haskell                                                         | Scala                                  |
|-----------------------------------------------------------------|----------------------------------------|
| `nix run .#fida-contracts:exe:fida-contracts-serialise -- ARGS` | `sbt "run ARGS"`                       |
| `nix run .#fida-contracts:test:fida-contracts-test`             | `sbt test`                             |
| `cabal repl`                                                    | `sbt console`                          |
| fourmolu, cabal-fmt, nixpkgs-fmt (`make format`)                | scalafmt (`make format`)               |
| plutus-simple-model tests                                       | Scalus emulator and direct evaluation  |

See [docs/MIGRATION.md](docs/MIGRATION.md) for the module map, the reasoning behind these choices,
the (malformed-data only) behavioural differences, and the issues inherited from the Haskell
contracts, which this migration deliberately does not fix.
