# Migration from Haskell (PlutusTx) to Scalus

The contracts were ported one-to-one from the Haskell/PlutusTx implementation (last Haskell
commit: `189a1a0`, "Shorten error codes across all validator contracts") to Scala 3 with
[Scalus](https://scalus.org) 1.4.1. The port keeps the on-chain behaviour, the data encodings and
the error codes; this document records what changed, what deliberately did not, and the issues
inherited from the original contracts.

## Module map

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
  produce smaller and cheaper scripts and make Scalus reject out-of-range constructor tags (see
  below), at the cost of requiring UPLC 1.1.0 support in the off-chain tooling.
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
- Test helper names and test names follow the Haskell toolbox. Negative tests now also assert the
  error code reported by the script.

## Behavioural differences (malformed data only)

For well-formed datums and redeemers the port accepts and rejects exactly the same transactions
as the Haskell contracts. The differences below only concern data that does not have the expected
shape; none of them gives an attacker a new capability.

1. **Lazy decoding.** PlutusTx's `unsafeFromBuiltinData` decoded a whole datum up front, so a
   malformed field anywhere made the script fail. Scalus' `to[T]` is a zero-cost retag: a field
   is only decoded when the validator reads it, so a malformed field the validator never reads is
   ignored. Likewise, when a validator rebuilds the expected continuing datum (e.g. the input datum
   with a new state), the fields it does not change are carried over as received, whereas PlutusTx
   re-encoded them canonically: for an input datum holding `Nothing` written as `Constr 1 [I 0]`,
   Haskell expected `Constr 1 []` in the output and the port expects `Constr 1 [I 0]`
   (`InsurancePolicyValidatorSpec` characterises this). The same goes for extra trailing fields,
   e.g. in a claim supplied by a `PolicyOnRiskCreateClaim` redeemer. Continuing datums are still
   compared byte-for-byte with that expected value.
2. **Out-of-range constructor tags.** At the protocol version 10 target, matching on a sum type
   treats an unknown tag as its last constructor, where Haskell failed to decode (e.g. a
   `PiggyBankRedeemer` with tag 9 behaves as `UnlockCollateral`), unless the match has a
   reachable `case _` (e.g. the `PolicyOnRiskRedeemer` match), which then rejects it. Such a
   redeemer only selects a branch that a well-formed redeemer could select anyway; such a datum is
   junk sent to a script address. Compiling for protocol version 11 makes these tags fail, as in
   Haskell.
3. **`fromBuiltinData` filters.** Where Haskell filtered outputs/reference inputs with
   `Just x <- [fromBuiltinData d]`, the port matches on the constructor tag; a datum with the
   expected tag but malformed fields now fails the script instead of being skipped.

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
10. **Unvalidated creation.** The `InsuranceId` minting policy only checks that its `TxOutRef` is
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
