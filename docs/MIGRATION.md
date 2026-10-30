# Migration from Haskell (PlutusTx) to Scalus

The contracts were ported one-to-one from the Haskell/PlutusTx implementation (last Haskell
commit: `189a1a0`, "Shorten error codes across all validator contracts") to Scala 3 with
[Scalus](https://scalus.org) 1.4.1. The port keeps the on-chain behaviour, the data encodings and
the error codes; this document records what changed, what deliberately did not, and the issues
inherited from the original contracts.

## Module map

The Haskell sources were removed by this migration; they remain in the git history, e.g.
`git show 189a1a0:src/Fida/Contract/Insurance.hs`.

| Haskell (`src/Fida/Contract/...`) | Scala (`src/main/scala/fida/contract/...`) |
|---|---|
| `Utils.hs` (+ `plutus-ledger-api` helpers) | `Utils.scala` |
| `SystemId.hs` | `SystemId.scala` (`SystemIdMintingPolicy`) |
| `Investor.hs` | `Investor.scala` (`InvestorValidator`) |
| `Insurance.hs` | `insurance/InsurancePolicy.scala` (`InsurancePolicyValidator`) |
| `Insurance/InsuranceId.hs` | `insurance/InsuranceId.scala` (`InsuranceIdMintingPolicy`) |
| `Insurance/Authority.hs` | `insurance/Authority.scala` |
| `Insurance/Datum.hs` | `insurance/Datum.scala` |
| `Insurance/Redeemer.hs` | `insurance/Redeemer.scala` |
| `Insurance/Tokens.hs` | `insurance/Tokens.scala` |
| `Insurance/PiggyBank.hs` | `insurance/PiggyBank.scala` (`PiggyBankValidator`) |
| `Insurance/Lifecycle/*.hs` | `insurance/lifecycle/*.scala` (`*Lifecycle`) |
| `app/serialise/Main.hs` | `fida/serialise/Main.scala` |
| `test/**` (plutus-simple-model) | `src/test/scala/**` (Scalus ledger emulator) |

`Scripts.scala` holds the compiled scripts (the Haskell `serialisable*` values) and applies
parameters.

## Decisions

- **Plutus V2 is kept.** The scripts keep the V2 script context and the
  datum/redeemer/context argument convention, so the off-chain code keeps working unchanged.
- **Protocol version 10 (Plomin) is the compilation target.** The scripts are therefore plain
  UPLC 1.0.0 programs (no `constr`/`case`), like the Haskell ones, which keeps them compatible
  with existing off-chain tooling (e.g. cardano-transaction-lib's `applyArgs`). They run unchanged
  on mainnet, preprod and preview, which are at protocol version 11 (van Rossem) since mid-2026.
  Targeting protocol version 11 (`MajorProtocolVersion.vanRossemPV` in `Scripts.options`) would
  produce smaller and cheaper scripts, at the cost of requiring UPLC 1.1.0 support in the
  off-chain tooling.
- **Encodings are unchanged.** Every datum, redeemer and parameter type produces exactly the
  `Data` the Haskell `makeIsDataIndexed`/newtype-derived instances produced
  (`DataEncodingSpec` pins them). In particular `PiggyBankRedeemer` is declared in on-chain index
  order (`PayForClaimWithCollateral` = 3, `ClaimPremiumOnCancel` = 4), which differs from the
  Haskell declaration order.
- **Parameters are applied like `applyArgs`.** The scripts are compiled unapplied, taking their
  parameters as `Data`; parameters are applied by wrapping the program in UPLC applications to
  `Data` constants, without recompiling.
- **Script hashes and addresses change**, because a different compiler produces different UPLC.
  Policies created with the Haskell scripts must keep using them.
- **Error codes are unchanged** (`ERR-INIT-1`, `ERR-PBANK-12`, ...). As with PlutusTx's `check`, a
  failing validator logs its code followed by `PT5`. Compiler-internal failures (e.g. a
  missing inline datum) may log Scalus' own messages instead of PlutusTx's `PT*` codes.
- **The redeemers of the minting policies are ignored** and the investor validator accepts every
  spend. This matches the compiled Haskell scripts, whose unused redeemer decoding was eliminated
  by GHC.
- **Unreachable branches.** The insurance policy dispatch has no default case: in the Haskell
  code, the fallthrough to `ERR-MAIN-0` after the `PolicyFunding`, `PolicyOnRisk` and
  `PolicyExpire` clauses cannot be reached (`ERR-MAIN-0` is still raised when a datum that is not
  `InsuranceInfo` expires). `ERR-PBANK-2`, just as unreachable, is kept and excluded from mutation
  testing.
- Test helper names and test names follow the Haskell toolbox. Negative tests now also assert the
  error code reported by the script.

## Decoding

Scalus' on-chain `to[T]` is a zero-cost retag, not a decoder, whereas the Haskell contracts relied
on PlutusTx decoding. To keep the behaviour on malformed data too, the port decodes explicitly:

- **Own datum and redeemer.** Both validators decode their parameters, datum and redeemer like
  PlutusTx `unsafeFromBuiltinData` in the Haskell wrappers: malformed data fails the script, an
  unknown constructor fails with `PT1`, extra constructor fields are ignored, and the values are
  rebuilt, so every expected continuing datum is re-encoded canonically, as `toBuiltinData` did.
- **Other UTxOs' datums.** Where the Haskell filtered outputs or reference inputs with
  `Just x <- [fromBuiltinData d]` (premium payments, sold fida cards, policy info, claim
  payments, piggy bank outputs), the port counts a datum only when PlutusTx `fromBuiltinData` would
  decode it: the right constructor, exactly its fields, each of them well formed. Anything else,
  including non-constructor data, is skipped rather than failing the script.

These emulations follow the instances PlutusTx 1.0 derives with `makeIsDataIndexed` and the
plutus-ledger-api V2 instances; the reviews compared them with the plutus 1.0.0 sources (the
Haskell build itself could not be run alongside). Expected datums are compared byte-for-byte with
`equalsData`, as the Haskell compared `BuiltinData`. With this, no behavioural difference is known
for well-formed or malformed datums and redeemers. The one known difference needs malformed
script parameters: the port decodes its parameters when the validator runs, whereas the Haskell
decoded them as soon as they were applied, so a validator instantiated with, say, an `I` as fida
card id and run with too few arguments (e.g. as a minting policy) fails in Haskell but not here.

An earlier iteration retagged the policy datum and redeemer lazily; the reviews showed that let a
policy holder store a claim with an extra field that later actions could not read, permanently
blocking collateral unlocks after a cancellation. `InsurancePolicyValidatorSpec` pins the strict
behaviour (canonical re-encoding, extra fields, malformed claims, unknown constructors).

## Known issues inherited from the Haskell contracts

The migration ports these faithfully; fixing them changes the protocol and is left as follow-up
work. The first three are demonstrated by `KnownIssuesTest` (pending until fixed).

1. **Premium payment replay (critical).** `PolicyInitiatedPayPremium` on an `InsuranceInfo` datum
   is accepted in any state, provided some input holds the `POLICY-PAYMENT` token. After the first
   premium payment that token is in the policy holder's wallet and can never be burned, so the
   holder can move an on-risk, expired or even cancelled policy (initiated redeemers are
   dispatched before the cancelled-state check) back to `Funding`, then complete funding again to
   restart the coverage without paying a new premium, re-locking investors' collateral.
2. **Selling a fida card back at any time (critical).** `SellFidaCard` checks neither the policy
   state nor the ADA leaving the piggy bank: the card holder can recover the whole collateral
   while the policy is on risk (even with an accepted claim unpaid), leaving the policy holder
   uncovered. Of the returned datum only `pbfcIsSold` and the card value are checked, so the
   seller can also rewrite `pbfcPaidClaims` and `pbfcFidaCardId`.
3. **Double satisfaction of claim payments (high).** `PayForClaimWithCollateral` sums all claim
   payment outputs of the transaction, so several piggy banks spent together are satisfied by one
   payment. After the claim's time to pay anyone may trigger the payment, so a third party can pocket
   the difference. `ClaimPremium` and `ClaimPremiumOnCancel` similarly measure the continuing
   outputs with an identical datum, so two premium UTxOs with the same datum at one piggy bank can
   share one continuing output.
4. **`AtLeastOneSign` means exactly one.** `AtLeastOneSign` is satisfied only when exactly one of
   its keys signs; a transaction signed by two of them is rejected (characterised in
   `LifecycleScenariosTest`).
5. **Missing state checks.** `PolicyFundingCancel`, `PolicyOnRiskCancel`, `PolicyOnRiskAcceptClaim`,
   `PolicyOnRiskCloseClaim`, `PolicyOnRiskFailClaim` and `PolicyOnRiskExpireClaim` do not check the
   policy state (only the cancelled state is excluded by the dispatcher); `BuyFidaCard` and
   `SellFidaCard` do not look at the policy at all.
6. **Claims.** `PolicyOnRiskCreateClaim` replaces any existing claim, including an accepted one;
   `PolicyOnRiskFailClaim` does not require a claim; `PolicyOnRiskExpireClaim` needs no signature
   and also expires accepted claims; `PolicyOnRiskClaimPayment` only requires the policy holder's
   signature, not where the funds go; `PolicyOnRiskFinalizeClaim` is not implemented (always
   rejected); `iInfoTotalClaimsAcceptedAmount` is never updated.
7. **Premium payment checks.** The premium payment info UTxO is not required to hold the
   `POLICY-PAYMENT` token; `ERR-INIT-4` counts policy info outputs whatever their datum; anyone may
   pay the premium and overpaying is accepted when the datum states the paid amount; a piggy bank
   listed twice in `ppInfoPiggyBanks` makes the premium impossible to pay.
8. **Unsold piggy banks can be emptied by anyone.** `UnlockCollateralOnCancel` and
   `UnlockCollateral` prove card ownership with `valueSpent`, which includes the piggy bank's own
   input; an unsold piggy bank still holds its card token, so anyone can spend it once the policy
   is cancelled or expired.
9. **Error codes.** `ERR-PBANK-8` is used both for a missing `SellFidaCard` output datum and for a
   wrong `ClaimPremium` amount; the fourth `ClaimPremiumOnCancel` check (`refund == 0`) reuses the
   fall-through code `ERR-PBANK-32`; a `BuyFidaCard` status output without inline datum fails
   without a code (incomplete Haskell `case`). `ERR-INIT-5` and `ERR-PBANK-1` are documented but
   never raised; `ERR-PBANK-2` is always preceded by the `Lf` failure of `getContinuingOutputs`;
   `ERR-PBANK-19` cannot happen in a spending context.
10. **Validators run for other purposes succeed.** Applied to the two arguments of a minting,
    rewarding or certifying script, the insurance policy and piggy bank validators return a
    function instead of failing, which the ledger counts as success, so anyone can mint tokens or
    use a stake credential under their script hashes. Nothing in the contracts gives those any
    meaning.
11. **Unvalidated creation.** The `InsuranceId` minting policy only checks that its `TxOutRef` is
   spent: the minted tokens and the initial datums are not validated on chain.

## Scalus notes for maintainers

- **Compiler issue (Scalus 1.4.1).** In a function with two or more parameters, a
  `d match { case Data.Constr(tag, fields) => ...; case _ => ... }` that uses `tag` or `fields`
  more than once evaluates `unConstrData(d)` outside its branch, so the script fails on `I`/`B`/
  `List`/`Map` data instead of reaching `case _`. Helpers that match on raw `Data` therefore take
  the `Data` as their only parameter.
- Compiled `a > b` and `a >= b` evaluate `b` first, and the Scalus inliner may move a single-use
  `val` into the comparison, so binding the operands to `val`s first does not fix the order. Where
  the reported error code depends on it, the contracts write `!(a < b)` instead.
- Lowering to UPLC happens when a script's `program` is first accessed, so some compilation
  errors only surface when the tests run (`ScriptsTest` lowers every script).
- On-chain `to[T]` does not validate; where the contracts must skip data that does not have the
  expected shape (plutus-tx `fromBuiltinData`), they check it explicitly.
- **Coverage and mutation testing run on the `jvm` build** (see the README). The Scalus plugin
  compiles `@Compile` objects after the Scala 3 coverage instrumentation, which it cannot compile
  (`Literals of type Int can't be used in Scalus scripts`), and the stryker4s mutation switches read
  the active mutant at run time, which UPLC cannot. Without the plugin the contracts are plain
  Scala. On the JVM, `to[T]` decodes eagerly, so a run on the JVM can fail where the script does
  not, on malformed data the script never decodes: the direct tests, which compare both runs, keep
  their script contexts well-formed.
- stryker4s mutates `===` into the cats operator `=!=`. `jvm/src` defines `=!=` for Scalus' `Eq`, in
  the `jvm` build only, so that these mutants compile and run.
