package fida.contract.insurance

import fida.contract.Utils.{
    before,
    check,
    findOwnInput,
    fromSingleton,
    getContinuingOutputs,
    lovelaceOf,
    lovelaceValueOf,
    outputsWithToken,
    referenceDatums,
    referenceOutputs,
    traceError,
    traceIfFalse,
    txSignedBy,
    unsafeFromSingletonOr,
    untypedOutput,
    valueOf,
    valueSpentOf
}
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PolicyClaimPayment}
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Expired}
import fida.contract.insurance.PiggyBankDatum.{
    PBankFidaCard,
    PBankPremium,
    unlockedPremiumToClaim,
    untypedSetFidaCardSold,
    untypedUpdatePiggyBankRefund
}
import fida.contract.insurance.PiggyBankRedeemer.*
import scalus.compiler.Compile
import scalus.uplc.builtin.{BuiltinList, Builtins, ByteString, Data}
import scalus.uplc.builtin.Data.{to, toData}
import scalus.cardano.onchain.plutus.v1.{Address, PolicyId}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext}
import scalus.cardano.onchain.plutus.prelude.*

/** Piggy bank validator (port of `Fida.Contract.Insurance.PiggyBank`).
  *
  * Parameters: the [[InsuranceId]] and the [[FidaCardId]] of the fida card the piggy bank backs. A
  * piggy bank UTxO holds either the collateral of that fida card (`PBankFidaCard`, marked by the
  * `CARD-STATUS` token) or a share of the premium paid by the policy holder (`PBankPremium`).
  *
  * Actions, in the order of the Haskell clauses; every other redeemer and datum combination fails
  * with `ERR-PBANK-32`:
  *   - `BuyFidaCard` on an unsold card: the status output must hold the card value on top of the
  *     input lovelace and carry the input datum marked sold.
  *   - `SellFidaCard` on a sold card: the continuing output holding the card and status tokens must
  *     carry an unsold datum with the same card value. Neither signatures nor payouts are checked.
  *   - `ClaimPremium` on a premium: the card owner may take the premium unlocked by the
  *     installments due (all of it once every installment is due), less refunds.
  *   - `PayForClaimWithCollateral` on a sold card: pays the card's share of the accepted claim to
  *     the insurance from the collateral and records the claim as paid.
  *   - `ClaimPremiumOnCancel` on a premium: refunds the premium not yet unlocked for investors to
  *     the holder of a cancelled policy, once.
  *   - `UnlockCollateralOnCancel` / `UnlockCollateral` on any card: the card owner takes the
  *     collateral back once the policy is cancelled / expired (with its claim, if any, paid).
  *
  * Decoding follows the Haskell code exactly: [[untyped]] decodes the parameters, datum and
  * redeemer strictly with `unsafeFromBuiltinData` semantics, and a datum the Haskell decodes with
  * `fromBuiltinData` (continuing outputs, reference inputs, claim payments) counts only when it
  * decodes exactly to the expected constructor.
  *
  * On-chain errors (`ERR-PBANK-1` is documented in the Haskell source but never raised):
  *   - `ERR-PBANK-0`: output datum doesn't say that the card is sold
  *   - `ERR-PBANK-2`: own input not found (unreachable: `getContinuingOutputs` fails first with
  *     `Lf`)
  *   - `ERR-PBANK-3`: correct output datum not found
  *   - `ERR-PBANK-4`: fida card not paid for
  *   - `ERR-PBANK-5`: output datum doesn't say that the card is not sold
  *   - `ERR-PBANK-6`: output datum doesn't match input datum (card value)
  *   - `ERR-PBANK-7`: not a fida card owner
  *   - `ERR-PBANK-8`: invalid claimed premium amount; also, for `SellFidaCard`, no single
  *     continuing output with the card and status tokens and a `PBankFidaCard` datum
  *   - `ERR-PBANK-9`: no reference input with insurance info (`.1` none, `.2` several)
  *   - `ERR-PBANK-10`: insurance policy has not been started (no start date)
  *   - `ERR-PBANK-11`: claim is not accepted
  *   - `ERR-PBANK-12`: incorrect collateral diff amount (too much collateral withdrawn)
  *   - `ERR-PBANK-13`: claim was already paid
  *   - `ERR-PBANK-14`: claim was not marked as paid
  *   - `ERR-PBANK-15`: paid for claim was not correct
  *   - `ERR-PBANK-16`: unauthorised access
  *   - `ERR-PBANK-17`: fida card value can't be changed
  *   - `ERR-PBANK-18`: no reference input with insurance info (with a claim)
  *   - `ERR-PBANK-19`: own consumed input not found (only in a non-spending context)
  *   - `ERR-PBANK-20`: no output with correct datum
  *   - `ERR-PBANK-21`: policy is not cancelled
  *   - `ERR-PBANK-22`: not signed by the policy holder
  *   - `ERR-PBANK-23`: invalid claimed premium amount
  *   - `ERR-PBANK-24`: no reference input with insurance info (`.1` none, `.2` several)
  *   - `ERR-PBANK-25`: policy is not cancelled
  *   - `ERR-PBANK-26`: not a fida card owner
  *   - `ERR-PBANK-27`: no reference input with insurance info (`.1` none, `.2` several)
  *   - `ERR-PBANK-28`: policy is not expired
  *   - `ERR-PBANK-29`: not a fida card owner
  *   - `ERR-PBANK-30`: claim was not paid
  *   - `ERR-PBANK-31`: no reference input with insurance info
  *   - `ERR-PBANK-32`: illegal action for the datum; also `ClaimPremiumOnCancel` on a premium that
  *     was already refunded
  */
@Compile
object PiggyBankValidator {

    def validate(
        iid: InsuranceId,
        fcid: FidaCardId,
        datum: PiggyBankDatum,
        redeemer: PiggyBankRedeemer,
        ctx: ScriptContext
    ): Boolean =
        val cs = iid.cs
        val n = fcid.id
        redeemer match
            case BuyFidaCard =>
                datum match
                    case PBankFidaCard(isSold, _, cardValue, _) =>
                        if isSold then traceError("ERR-PBANK-32")
                        else buyFidaCard(cs, datum, cardValue, ctx)
                    case _ => traceError("ERR-PBANK-32")
            case SellFidaCard =>
                datum match
                    case PBankFidaCard(isSold, _, cardValue, _) =>
                        if isSold then sellFidaCard(cs, n, cardValue, ctx)
                        else traceError("ERR-PBANK-32")
                    case _ => traceError("ERR-PBANK-32")
            case ClaimPremium =>
                datum match
                    case PBankPremium(initAmount, refund) =>
                        claimPremium(cs, n, initAmount, refund, ctx)
                    case _ => traceError("ERR-PBANK-32")
            case PayForClaimWithCollateral =>
                datum match
                    case PBankFidaCard(isSold, _, cardValue, paidClaims) =>
                        if isSold then payForClaimWithCollateral(cs, n, cardValue, paidClaims, ctx)
                        else traceError("ERR-PBANK-32")
                    case _ => traceError("ERR-PBANK-32")
            case ClaimPremiumOnCancel =>
                datum match
                    case PBankPremium(initAmount, refund) =>
                        claimPremiumOnCancel(cs, datum, initAmount, refund, ctx)
                    case _ => traceError("ERR-PBANK-32")
            case UnlockCollateralOnCancel =>
                datum match
                    case PBankFidaCard(_, _, _, _) => unlockCollateralOnCancel(cs, n, ctx)
                    case _                         => traceError("ERR-PBANK-32")
            case UnlockCollateral =>
                datum match
                    case PBankFidaCard(_, _, _, paidClaims) =>
                        unlockCollateral(cs, n, paidClaims, ctx)
                    case _ => traceError("ERR-PBANK-32")

    /** Entry point, the equivalent of the Haskell `mkPiggyBankValidatorUntyped`.
      *
      * Like PlutusTx `unsafeFromBuiltinData` in the Haskell wrapper, decodes the parameters, then
      * the datum, then the redeemer strictly before validating: malformed data fails the script, an
      * unknown constructor fails with `PT1`, and extra constructor fields are ignored. The datum
      * and redeemer are rebuilt from the decoded values, so re-encoding them gives the canonical
      * encoding, as `toBuiltinData` does in the Haskell code. The script context comes from the
      * ledger and is well-formed. A false result fails with `PT5` (PlutusTx `check`).
      */
    def untyped(iid: Data, fcid: Data, datum: Data, redeemer: Data, ctx: Data): Unit =
        val insuranceId = InsuranceId(Builtins.unBData(iid))
        val fidaCardId = FidaCardId(Builtins.unBData(fcid))
        val decodedDatum = decodePiggyBankDatum(datum)
        val decodedRedeemer = decodePiggyBankRedeemer(redeemer)
        check(
          validate(insuranceId, fidaCardId, decodedDatum, decodedRedeemer, ctx.to[ScriptContext])
        )

    // ---------------------------------------------------------------------------------------------
    // Actions

    /** `BuyFidaCard` on `PBankFidaCard {pbfcIsSold = False}`. */
    private def buyFidaCard(
        cs: PolicyId,
        datum: PiggyBankDatum,
        cardValue: BigInt,
        ctx: ScriptContext
    ): Boolean =
        val outputDatum = untypedOutput(cs, ctx, Tokens.fidaCardStatusTokenName) match
            case Option.None      => traceError("ERR-PBANK-3")
            case Option.Some(out) =>
                out.datum match
                    case OutputDatum.OutputDatum(d) =>
                        val outputLovelace = lovelaceValueOf(out.value)
                        val inputLovelace = findOwnInput(ctx) match
                            case Option.Some(input) => lovelaceValueOf(input.resolved.value)
                            case Option.None        => traceError("ERR-PBANK-2")
                        if outputLovelace >= cardValue + inputLovelace then d
                        else traceError("ERR-PBANK-4")
                    // the Haskell `case` has no alternative for an output without inline datum
                    case _ => fail()
        traceIfFalse("ERR-PBANK-0", Option.Some(outputDatum) === untypedSetFidaCardSold(datum))

    /** `SellFidaCard` on `PBankFidaCard {pbfcIsSold = True}`. */
    private def sellFidaCard(
        cs: PolicyId,
        n: ByteString,
        cardValue: BigInt,
        ctx: ScriptContext
    ): Boolean =
        val cardToken = Tokens.fidaCardTokenName(n)
        val returned = fromSingleton(
          getContinuingOutputs(ctx).filterMap { out =>
              out.datum match
                  case OutputDatum.OutputDatum(d) =>
                      if valueOf(out.value, cs, cardToken) === BigInt(1)
                          && valueOf(out.value, cs, Tokens.fidaCardStatusTokenName) === BigInt(1)
                          && isPiggyBankDatumData(d)
                      then Option.Some(d.to[PiggyBankDatum])
                      else Option.None
                  case _ => Option.None
          }
        )
        returned match
            case Option.Some(outputDatum) =>
                outputDatum match
                    case PBankFidaCard(outputIsSold, _, outputCardValue, _) =>
                        traceIfFalse("ERR-PBANK-5", !outputIsSold)
                        && traceIfFalse("ERR-PBANK-6", cardValue === outputCardValue)
                    case _ => traceError("ERR-PBANK-8")
            case Option.None => traceError("ERR-PBANK-8")

    /** `ClaimPremium` on `PBankPremium initAmount refund`. */
    private def claimPremium(
        cs: PolicyId,
        n: ByteString,
        initAmount: BigInt,
        refund: BigInt,
        ctx: ScriptContext
    ): Boolean =
        val txInfo = ctx.txInfo
        traceIfFalse(
          "ERR-PBANK-7",
          valueSpentOf(txInfo, cs, Tokens.fidaCardTokenName(n)) === BigInt(1)
        )
        && {
            // `toBuiltinData datum`: the re-encoded input datum
            val datumData = PBankPremium(initAmount, refund).toData
            val lockedPremium = lovelaceOf(
              getContinuingOutputs(ctx).filter { out =>
                  out.datum match
                      case OutputDatum.OutputDatum(d) => d === datumData
                      case _                          => false
              }
            )
            val availablePremium =
                unsafeFromSingletonOr("ERR-PBANK-9", insuranceInfos(cs, ctx)) match
                    case InsuranceInfo(
                          _,
                          _,
                          _,
                          _,
                          installments,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          startDate,
                          _
                        ) =>
                        startDate match
                            case Option.Some(start) =>
                                unlockedPremiumToClaim(
                                  txInfo.validRange,
                                  initAmount,
                                  installments,
                                  start
                                )
                            case Option.None => traceError("ERR-PBANK-10")
                    case _ => fail()
            // `lockedPremium >= initAmount - refund - availablePremium`, evaluating `lockedPremium`
            // first as the Haskell does (`>=` evaluates its right operand first, and the optimizer
            // inlines both bindings into the comparison)
            traceIfFalse("ERR-PBANK-8", !(lockedPremium < initAmount - refund - availablePremium))
        }

    /** `PayForClaimWithCollateral` on `PBankFidaCard {pbfcIsSold = True}`. */
    private def payForClaimWithCollateral(
        cs: PolicyId,
        n: ByteString,
        cardValue: BigInt,
        paidClaims: List[ByteString],
        ctx: ScriptContext
    ): Boolean =
        // `referenceOutputs` decoded as any `InsurancePolicyDatum`: exactly one, an `InsuranceInfo`
        // with a claim
        val policyInfos = referenceOutputs(cs, ctx, Tokens.policyInfoTokenName)
            .filter(ref => isInsurancePolicyDatumData(ref._2))
        policyInfos match
            case List.Cons(ref, List.Nil) =>
                ref._2.to[InsurancePolicyDatum] match
                    case InsuranceInfo(
                          _,
                          _,
                          fidaCardNumber,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          _,
                          claimTimeToPay,
                          _,
                          claim
                        ) =>
                        claim match
                            case Option.Some(c) =>
                                payClaimShare(
                                  cs,
                                  n,
                                  cardValue,
                                  paidClaims,
                                  ref._1.address,
                                  c,
                                  fidaCardNumber,
                                  claimTimeToPay,
                                  ctx
                                )
                            case Option.None => traceError("ERR-PBANK-18")
                    case _ => traceError("ERR-PBANK-18")
            case _ => traceError("ERR-PBANK-18")

    /** The checks of `PayForClaimWithCollateral`, given the policy info. */
    private def payClaimShare(
        cs: PolicyId,
        n: ByteString,
        cardValue: BigInt,
        paidClaims: List[ByteString],
        insuranceAddress: Address,
        claim: ClaimInfo,
        fidaCardNumber: BigInt,
        claimTimeToPay: DiffMilliSeconds,
        ctx: ScriptContext
    ): Boolean =
        val txInfo = ctx.txInfo
        traceIfFalse("ERR-PBANK-11", claim.claimAccepted)
        && {
            val inputCollateral = findOwnInput(ctx) match
                case Option.Some(input) => lovelaceValueOf(input.resolved.value)
                case Option.None        => traceError("ERR-PBANK-19")
            // `output cs sc fidaCardStatusTokenName` decoded as a `PiggyBankDatum`
            val statusOutputs =
                outputsWithToken(getContinuingOutputs(ctx), cs, Tokens.fidaCardStatusTokenName)
                    .filter(output => isPiggyBankDatumData(output._2))
            fromSingleton(statusOutputs) match
                case Option.Some(output) =>
                    output._2.to[PiggyBankDatum] match
                        case PBankFidaCard(_, _, outputCardValue, outputPaidClaims) =>
                            val outputCollateral = lovelaceValueOf(output._1.value)
                            val claimAmount = claim.claimAmount
                            val claimId = claim.claimId
                            traceIfFalse(
                              "ERR-PBANK-12",
                              claimAmount >= fidaCardNumber * (inputCollateral - outputCollateral)
                            )
                            && traceIfFalse("ERR-PBANK-13", !paidClaims.contains(claimId))
                            && traceIfFalse(
                              "ERR-PBANK-14",
                              List.Cons(claimId, paidClaims) === outputPaidClaims
                            )
                            && traceIfFalse(
                              "ERR-PBANK-15",
                              fidaCardNumber * paidToInsurance(insuranceAddress, ctx) >= claimAmount
                            )
                            && traceIfFalse(
                              "ERR-PBANK-16",
                              before(claim.claimDate + claimTimeToPay, txInfo.validRange)
                                  || valueSpentOf(
                                    txInfo,
                                    cs,
                                    Tokens.fidaCardTokenName(n)
                                  ) === BigInt(1)
                            )
                            && traceIfFalse("ERR-PBANK-17", outputCardValue === cardValue)
                        case _ => traceError("ERR-PBANK-20")
                case Option.None => traceError("ERR-PBANK-20")
        }

    /** Lovelace paid to `insuranceAddress` with a `PolicyClaimPayment` inline datum. */
    private def paidToInsurance(insuranceAddress: Address, ctx: ScriptContext): BigInt =
        val claimPayment = PolicyClaimPayment.toData
        lovelaceOf(
          ctx.txInfo.outputs.filter { out =>
              out.datum match
                  case OutputDatum.OutputDatum(d) =>
                      insuranceAddress === out.address && d === claimPayment
                  case _ => false
          }
        )

    /** `ClaimPremiumOnCancel` on `PBankPremium initAmount refund`. */
    private def claimPremiumOnCancel(
        cs: PolicyId,
        datum: PiggyBankDatum,
        initAmount: BigInt,
        refund: BigInt,
        ctx: ScriptContext
    ): Boolean =
        val txInfo = ctx.txInfo
        unsafeFromSingletonOr("ERR-PBANK-24", insuranceInfos(cs, ctx)) match
            case InsuranceInfo(
                  _,
                  _,
                  _,
                  _,
                  installments,
                  _,
                  policyHolder,
                  _,
                  state,
                  _,
                  _,
                  _,
                  _,
                  startDate,
                  _
                ) =>
                traceIfFalse("ERR-PBANK-21", state === Cancelled)
                && traceIfFalse("ERR-PBANK-22", txSignedBy(txInfo, policyHolder))
                && {
                    val spendValue = findOwnInput(ctx) match
                        case Option.Some(input) => lovelaceValueOf(input.resolved.value)
                        case Option.None        => BigInt(0)
                    val premiumLeftForInvestor = startDate match
                        case Option.Some(start) =>
                            unlockedPremiumToClaim(
                              txInfo.validRange,
                              initAmount,
                              installments,
                              start
                            )
                        case Option.None => BigInt(0)
                    val refundAmount = initAmount - premiumLeftForInvestor
                    val refundedDatum = untypedUpdatePiggyBankRefund(refundAmount, datum)
                    val lockedPremium = lovelaceOf(
                      getContinuingOutputs(ctx).filter { out =>
                          out.datum match
                              case OutputDatum.OutputDatum(d) => Option.Some(d) === refundedDatum
                              case _                          => false
                      }
                    )
                    traceIfFalse("ERR-PBANK-23", spendValue - lockedPremium <= refundAmount)
                }
                && traceIfFalse("ERR-PBANK-32", refund === BigInt(0))
            case _ => fail()

    /** `UnlockCollateralOnCancel` on any `PBankFidaCard`. */
    private def unlockCollateralOnCancel(cs: PolicyId, n: ByteString, ctx: ScriptContext): Boolean =
        val isPolicyCancelled = unsafeFromSingletonOr("ERR-PBANK-27", insuranceInfos(cs, ctx)) match
            case InsuranceInfo(_, _, _, _, _, _, _, _, state, _, _, _, _, _, _) =>
                state === Cancelled
            case _ => fail()
        traceIfFalse("ERR-PBANK-25", isPolicyCancelled)
        && traceIfFalse(
          "ERR-PBANK-26",
          valueSpentOf(ctx.txInfo, cs, Tokens.fidaCardTokenName(n)) === BigInt(1)
        )

    /** `UnlockCollateral` on any `PBankFidaCard`. */
    private def unlockCollateral(
        cs: PolicyId,
        n: ByteString,
        paidClaims: List[ByteString],
        ctx: ScriptContext
    ): Boolean =
        insuranceInfos(cs, ctx) match
            case List.Cons(policyInfo, List.Nil) =>
                policyInfo match
                    case InsuranceInfo(_, _, _, _, _, _, _, _, state, _, _, _, _, _, claim) =>
                        traceIfFalse("ERR-PBANK-28", state === Expired)
                        && traceIfFalse(
                          "ERR-PBANK-29",
                          valueSpentOf(ctx.txInfo, cs, Tokens.fidaCardTokenName(n)) === BigInt(1)
                        )
                        && traceIfFalse("ERR-PBANK-30", isClaimPaid(claim, paidClaims))
                    case _ => fail()
            case _ => traceError("ERR-PBANK-31")

    /** `fromMaybe True (((`elem` pbfcPaidClaims) . claimId) <$> mClaim)` */
    private def isClaimPaid(claim: Option[ClaimInfo], paidClaims: List[ByteString]): Boolean =
        claim match
            case Option.Some(c) => paidClaims.contains(c.claimId)
            case Option.None    => true

    /** `[info | info@InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName]`: inline
      * datums of the reference inputs holding exactly one policy info token that decode as an
      * `InsuranceInfo`.
      */
    private def insuranceInfos(cs: PolicyId, ctx: ScriptContext): List[InsurancePolicyDatum] =
        referenceDatums(cs, ctx, Tokens.policyInfoTokenName).filterMap { d =>
            if isInsuranceInfoData(d) then Option.Some(d.to[InsurancePolicyDatum]) else Option.None
        }

    // ---------------------------------------------------------------------------------------------
    // Strict decoding of the datum and redeemer: PlutusTx `unsafeFromBuiltinData`, generated by
    // `makeIsDataIndexed` (an unknown constructor fails with `PT1`, extra fields are ignored)

    /** `unsafeFromBuiltinData @PiggyBankDatum`, rebuilding the datum from the decoded fields. */
    private def decodePiggyBankDatum(d: Data): PiggyBankDatum =
        val constr = Builtins.unConstrData(d)
        val tag = constr.fst
        val f0 = constr.snd
        if tag === BigInt(0) then
            val premiumInit = Builtins.unIData(Builtins.headList(f0))
            val premiumRefund = Builtins.unIData(Builtins.headList(Builtins.tailList(f0)))
            PBankPremium(premiumInit, premiumRefund)
        else if tag === BigInt(1) then
            val isSold = decodeBool(Builtins.headList(f0))
            val f1 = Builtins.tailList(f0)
            val cardId = Builtins.unBData(Builtins.headList(f1))
            val f2 = Builtins.tailList(f1)
            val cardValue = Builtins.unIData(Builtins.headList(f2))
            val paidClaims = decodeByteStrings(
              Builtins.unListData(Builtins.headList(Builtins.tailList(f2)))
            )
            PBankFidaCard(isSold, FidaCardId(cardId), cardValue, paidClaims)
        else traceError("PT1")

    /** `unsafeFromBuiltinData @Bool` (`False` 0, `True` 1). */
    private def decodeBool(d: Data): Boolean =
        val tag = Builtins.unConstrData(d).fst
        if tag === BigInt(0) then false
        else if tag === BigInt(1) then true
        else traceError("PT1")

    /** `unsafeFromBuiltinData @[BuiltinByteString]`. */
    private def decodeByteStrings(xs: BuiltinList[Data]): List[ByteString] =
        if Builtins.nullList(xs) then List.Nil
        else
            List.Cons(
              Builtins.unBData(Builtins.headList(xs)),
              decodeByteStrings(Builtins.tailList(xs))
            )

    /** `unsafeFromBuiltinData @PiggyBankRedeemer`: constructors 0 to 6, fields ignored. */
    private def decodePiggyBankRedeemer(d: Data): PiggyBankRedeemer =
        val tag = Builtins.unConstrData(d).fst
        if BigInt(0) <= tag && tag <= BigInt(6) then
            Builtins.constrData(tag, Builtins.mkNilData()).to[PiggyBankRedeemer]
        else traceError("PT1")

    // ---------------------------------------------------------------------------------------------
    // Exact decoding: whether PlutusTx `fromBuiltinData`, generated by `makeIsDataIndexed`, succeeds.
    // It requires a known constructor with exactly its fields, each of which decodes.

    private def isConstrData(d: Data): Boolean =
        Builtins.chooseData(d, true, false, false, false, false)

    private def isIntData(d: Data): Boolean =
        Builtins.chooseData(d, false, false, false, true, false)

    private def isBytesData(d: Data): Boolean =
        Builtins.chooseData(d, false, false, false, false, true)

    /** `[a]`: a list whose elements all decode. */
    private def isListOf(d: Data, isA: Data => Boolean): Boolean =
        def all(xs: BuiltinList[Data]): Boolean =
            Builtins.nullList(xs) || (isA(Builtins.headList(xs)) && all(Builtins.tailList(xs)))
        Builtins.chooseData(d, false, false, true, false, false) && all(Builtins.unListData(d))

    /** Whether `fields` has exactly `n` elements. */
    private def hasArity(fields: BuiltinList[Data], n: BigInt): Boolean =
        if Builtins.nullList(fields) then n === BigInt(0)
        else n > BigInt(0) && hasArity(Builtins.tailList(fields), n - 1)

    /** `Bool`: `False` 0 or `True` 1, without fields. */
    private def isBoolData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            Builtins.nullList(constr.snd) && (tag === BigInt(0) || tag === BigInt(1))
        }

    /** `Maybe a`: `Just` 0 `[a]` or `Nothing` 1 `[]`. */
    private def isMaybeOf(d: Data, isA: Data => Boolean): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            val fields = constr.snd
            if tag === BigInt(0) then hasArity(fields, 1) && isA(Builtins.headList(fields))
            else tag === BigInt(1) && Builtins.nullList(fields)
        }

    /** `PiggyBankDatum`: `PBankPremium` 0 `[Integer, Integer]` or `PBankFidaCard` 1
      * `[Bool, FidaCardId, Integer, [BuiltinByteString]]`.
      */
    private def isPiggyBankDatumData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            val f0 = constr.snd
            if tag === BigInt(0) then
                hasArity(f0, 2) && isIntData(Builtins.headList(f0))
                && isIntData(Builtins.headList(Builtins.tailList(f0)))
            else
                tag === BigInt(1) && hasArity(f0, 4) && {
                    val f1 = Builtins.tailList(f0)
                    val f2 = Builtins.tailList(f1)
                    val f3 = Builtins.tailList(f2)
                    isBoolData(Builtins.headList(f0))
                    && isBytesData(Builtins.headList(f1))
                    && isIntData(Builtins.headList(f2))
                    && isListOf(Builtins.headList(f3), isBytesData)
                }
        }

    /** `InsurancePolicyDatum`: `InsuranceInfo` 0, `PremiumPaymentInfo` 1 or `PolicyClaimPayment` 2.
      */
    private def isInsurancePolicyDatumData(d: Data): Boolean =
        isInsuranceInfoData(d) || isPremiumPaymentInfoData(d) || d === PolicyClaimPayment.toData

    /** `InsuranceInfo` 0 with its 15 fields. */
    private def isInsuranceInfoData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val f0 = constr.snd
            constr.fst === BigInt(0) && hasArity(f0, 15) && {
                val f1 = Builtins.tailList(f0)
                val f2 = Builtins.tailList(f1)
                val f3 = Builtins.tailList(f2)
                val f4 = Builtins.tailList(f3)
                val f5 = Builtins.tailList(f4)
                val f6 = Builtins.tailList(f5)
                val f7 = Builtins.tailList(f6)
                val f8 = Builtins.tailList(f7)
                val f9 = Builtins.tailList(f8)
                val f10 = Builtins.tailList(f9)
                val f11 = Builtins.tailList(f10)
                val f12 = Builtins.tailList(f11)
                val f13 = Builtins.tailList(f12)
                val f14 = Builtins.tailList(f13)
                isIntData(Builtins.headList(f0)) // iInfoCollateralAmount
                && isIntData(Builtins.headList(f1)) // iInfoFidaCardValue
                && isIntData(Builtins.headList(f2)) // iInfoFidaCardNumber
                && isIntData(Builtins.headList(f3)) // iInfoPremiumAmount
                && isInstallmentsInfoData(Builtins.headList(f4)) // iInfoInstallments
                && isIntData(Builtins.headList(f5)) // iInfoInsurancePeriod
                && isBytesData(Builtins.headList(f6)) // iInfoPolicyHolder
                && isAuthorityData(Builtins.headList(f7)) // iInfoPolicyAuthority
                && isPolicyStateData(Builtins.headList(f8)) // iInfoState
                && isIntData(Builtins.headList(f9)) // iInfoFundingDeadline
                && isIntData(Builtins.headList(f10)) // iInfoTotalClaimsAcceptedAmount
                && isIntData(Builtins.headList(f11)) // iInfoClaimTimeToLive
                && isIntData(Builtins.headList(f12)) // iInfoClaimTimeToPay
                && isMaybeOf(Builtins.headList(f13), isIntData) // iInfoStartDate
                && isMaybeOf(Builtins.headList(f14), isClaimInfoData) // iInfoClaim
            }
        }

    /** `InstallmentsInfo` 0 `[[DiffMilliSeconds], Integer]`. */
    private def isInstallmentsInfoData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val f0 = constr.snd
            constr.fst === BigInt(0) && hasArity(f0, 2)
            && isListOf(Builtins.headList(f0), isIntData)
            && isIntData(Builtins.headList(Builtins.tailList(f0)))
        }

    /** `InsuranceAuthority`: `SingleSign` 0 `[PubKeyHash]`, or `AtLeastOneSign` 1, `AllMustSign` 2,
      * `MajorityMustSign` 3 `[[PubKeyHash]]`.
      */
    private def isAuthorityData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            val f0 = constr.snd
            hasArity(f0, 1) && {
                val field = Builtins.headList(f0)
                if tag === BigInt(0) then isBytesData(field)
                else
                    (tag === BigInt(1) || tag === BigInt(2) || tag === BigInt(3)) && isListOf(
                      field,
                      isBytesData
                    )
            }
        }

    /** `InsurancePolicyState`: `Initiated` 0 to `Expired` 4, without fields. */
    private def isPolicyStateData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            Builtins.nullList(constr.snd) && BigInt(0) <= tag && tag <= BigInt(4)
        }

    /** `ClaimInfo` 0 `[Integer, POSIXTime, BuiltinByteString, Bool, BuiltinByteString]`. */
    private def isClaimInfoData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val f0 = constr.snd
            constr.fst === BigInt(0) && hasArity(f0, 5) && {
                val f1 = Builtins.tailList(f0)
                val f2 = Builtins.tailList(f1)
                val f3 = Builtins.tailList(f2)
                val f4 = Builtins.tailList(f3)
                isIntData(Builtins.headList(f0)) // claimAmount
                && isIntData(Builtins.headList(f1)) // claimDate
                && isBytesData(Builtins.headList(f2)) // claimReason
                && isBoolData(Builtins.headList(f3)) // claimAccepted
                && isBytesData(Builtins.headList(f4)) // claimId
            }
        }

    /** `PremiumPaymentInfo` 1 `[Integer, [Address]]`. */
    private def isPremiumPaymentInfoData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val f0 = constr.snd
            constr.fst === BigInt(1) && hasArity(f0, 2)
            && isIntData(Builtins.headList(f0))
            && isListOf(Builtins.headList(Builtins.tailList(f0)), isAddressData)
        }

    /** `Address` 0 `[Credential, Maybe StakingCredential]`. */
    private def isAddressData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val f0 = constr.snd
            constr.fst === BigInt(0) && hasArity(f0, 2)
            && isCredentialData(Builtins.headList(f0))
            && isMaybeOf(Builtins.headList(Builtins.tailList(f0)), isStakingCredentialData)
        }

    /** `Credential`: `PubKeyCredential` 0 or `ScriptCredential` 1, with one hash. */
    private def isCredentialData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            val f0 = constr.snd
            (tag === BigInt(0) || tag === BigInt(1)) && hasArity(f0, 1) && isBytesData(
              Builtins.headList(f0)
            )
        }

    /** `StakingCredential`: `StakingHash` 0 `[Credential]` or `StakingPtr` 1
      * `[Integer, Integer, Integer]`.
      */
    private def isStakingCredentialData(d: Data): Boolean =
        isConstrData(d) && {
            val constr = Builtins.unConstrData(d)
            val tag = constr.fst
            val f0 = constr.snd
            if tag === BigInt(0) then hasArity(f0, 1) && isCredentialData(Builtins.headList(f0))
            else
                tag === BigInt(1) && hasArity(f0, 3) && {
                    val f1 = Builtins.tailList(f0)
                    isIntData(Builtins.headList(f0))
                    && isIntData(Builtins.headList(f1))
                    && isIntData(Builtins.headList(Builtins.tailList(f1)))
                }
        }
}
