package fida.contract.insurance.lifecycle

import fida.contract.Utils.{
    before,
    referenceDatums,
    traceIfFalse,
    traceIfNotSingleton,
    txSignedBy,
    untypedOutputDatum
}
import fida.contract.insurance.{InsuranceId, InsurancePolicyDatum, PolicyOnRiskRedeemer, Tokens}
import fida.contract.insurance.InsuranceAuthority.isSignedByTheAuthority
import fida.contract.insurance.InsurancePolicyDatum.{
    InsuranceInfo,
    PolicyClaimPayment,
    untypedUnsetClaim,
    untypedUpdateClaim,
    untypedUpdatePolicyState
}
import fida.contract.insurance.InsurancePolicyState.{Cancelled, OnRisk}
import fida.contract.insurance.PolicyOnRiskRedeemer.*
import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, Data}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v1.PolicyId
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.*

/** OnRisk policy actions (port of `Fida.Contract.Insurance.Lifecycle.OnRisk`).
  *
  * Clauses, tried in the Haskell order (only CreateClaim checks the policy state):
  *   - CreateClaim `c` (`InsuranceInfo` in state OnRisk): the policy info holds `c`, which is not
  *     accepted and dated before the validity range, signed by the policy holder;
  *   - AcceptClaim (`InsuranceInfo` with a claim): signed by the policy authority, the claim is
  *     marked accepted;
  *   - CloseClaim (`InsuranceInfo` with a claim): signed by the policy authority or the policy
  *     holder, the claim is unset;
  *   - FailClaim (`InsuranceInfo`, claim optional): signed by the policy authority, the claim is
  *     unset;
  *   - Cancel (`InsuranceInfo`): signed by the policy authority, the policy moves to Cancelled;
  *   - ExpireClaim (`InsuranceInfo` with a claim): the claim is unset once its date plus the claim
  *     time to live is before the validity range;
  *   - ClaimPayment (`PolicyClaimPayment`): exactly one reference input holding the POLICY-INFO NFT
  *     has an `InsuranceInfo` datum whose policy holder signs.
  *
  * On-chain errors:
  *   - `ERR-ON-RISK-0`: output datum doesn't match the updated datum (or the claim is accepted, or
  *     not dated before the validity range)
  *   - `ERR-ON-RISK-1`: not signed by the policy holder
  *   - `ERR-ON-RISK-2`: not signed by the policy authority
  *   - `ERR-ON-RISK-3`: output datum doesn't match the updated datum
  *   - `ERR-ON-RISK-4`: not signed by the policy holder or by the policy authority
  *   - `ERR-ON-RISK-5`: output datum doesn't match the updated datum
  *   - `ERR-ON-RISK-6`: not signed by the policy authority
  *   - `ERR-ON-RISK-7`: output datum doesn't match the updated datum
  *   - `ERR-ON-RISK-8`: not signed by the policy authority
  *   - `ERR-ON-RISK-9`: output datum doesn't match the updated datum
  *   - `ERR-ON-RISK-10`: output datum doesn't match the updated datum
  *   - `ERR-ON-RISK-11`: claim not expired yet
  *   - `ERR-ON-RISK-12`: not signed by the policy holder
  *   - `ERR-ON-RISK-13`: illegal action for the OnRisk state
  */
@Compile
object OnRiskLifecycle {

    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: PolicyOnRiskRedeemer,
        ctx: ScriptContext
    ): Boolean =
        datum match
            case InsuranceInfo(
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  policyHolder,
                  authority,
                  state,
                  _,
                  _,
                  claimTimeToLive,
                  _,
                  _,
                  claim
                ) =>
                redeemer match
                    case PolicyOnRiskCreateClaim(newClaim) =>
                        if state === OnRisk then
                            val txInfo = ctx.txInfo
                            traceIfFalse(
                              "ERR-ON-RISK-0",
                              policyInfoOutput(iid, ctx) === untypedUpdateClaim(datum, newClaim)
                                  && newClaim.claimAccepted === false
                                  && before(newClaim.claimDate, txInfo.validRange)
                            )
                            && traceIfFalse("ERR-ON-RISK-1", txSignedBy(txInfo, policyHolder))
                        else illegalAction
                    case PolicyOnRiskAcceptClaim =>
                        claim match
                            case Option.Some(c) =>
                                traceIfFalse(
                                  "ERR-ON-RISK-2",
                                  isSignedByTheAuthority(ctx, authority)
                                )
                                && traceIfFalse(
                                  "ERR-ON-RISK-3",
                                  policyInfoOutput(iid, ctx)
                                      === untypedUpdateClaim(datum, c.copy(claimAccepted = true))
                                )
                            case Option.None => illegalAction
                    case PolicyOnRiskCloseClaim =>
                        claim match
                            case Option.Some(_) =>
                                traceIfFalse(
                                  "ERR-ON-RISK-4",
                                  isSignedByTheAuthority(ctx, authority) || txSignedBy(
                                    ctx.txInfo,
                                    policyHolder
                                  )
                                )
                                && traceIfFalse(
                                  "ERR-ON-RISK-5",
                                  policyInfoOutput(iid, ctx) === untypedUnsetClaim(datum)
                                )
                            case Option.None => illegalAction
                    case PolicyOnRiskFailClaim =>
                        traceIfFalse("ERR-ON-RISK-6", isSignedByTheAuthority(ctx, authority))
                        && traceIfFalse(
                          "ERR-ON-RISK-7",
                          policyInfoOutput(iid, ctx) === untypedUnsetClaim(datum)
                        )
                    case PolicyOnRiskCancel =>
                        traceIfFalse("ERR-ON-RISK-8", isSignedByTheAuthority(ctx, authority))
                        && traceIfFalse(
                          "ERR-ON-RISK-9",
                          policyInfoOutput(iid, ctx) === untypedUpdatePolicyState(datum, Cancelled)
                        )
                    case PolicyOnRiskExpireClaim =>
                        claim match
                            case Option.Some(c) =>
                                traceIfFalse(
                                  "ERR-ON-RISK-10",
                                  policyInfoOutput(iid, ctx) === untypedUnsetClaim(datum)
                                )
                                && traceIfFalse(
                                  "ERR-ON-RISK-11",
                                  before(c.claimDate + claimTimeToLive, ctx.txInfo.validRange)
                                )
                            case Option.None => illegalAction
                    case _ => illegalAction // FinalizeClaim, ClaimPayment
            case PolicyClaimPayment =>
                redeemer match
                    case PolicyOnRiskClaimPayment => claimPaymentSignedByPolicyHolder(iid.cs, ctx)
                    case _                        => illegalAction
            case _ => illegalAction // PremiumPaymentInfo

    /** Haskell `trace "ERR-ON-RISK-13" False`. */
    private inline def illegalAction: Boolean = Builtins.trace("ERR-ON-RISK-13")(false)

    /** Haskell `untypedOutputDatum cs sc policyInfoTokenName`. */
    private inline def policyInfoOutput(iid: InsuranceId, ctx: ScriptContext): Option[Data] =
        untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)

    /** `traceIfNotSingleton "ERR-ON-RISK-12"` over the reference inputs holding exactly one
      * POLICY-INFO token whose inline datum decodes to an `InsuranceInfo` signed by its policy
      * holder.
      */
    private def claimPaymentSignedByPolicyHolder(cs: PolicyId, ctx: ScriptContext): Boolean =
        val txInfo = ctx.txInfo
        traceIfNotSingleton(
          "ERR-ON-RISK-12",
          referenceDatums(cs, ctx, Tokens.policyInfoTokenName).filterMap { d =>
              if isInsuranceInfo(d) then
                  d.to[InsurancePolicyDatum] match
                      case InsuranceInfo(_, _, _, _, _, _, holder, _, _, _, _, _, _, _, _) =>
                          if txSignedBy(txInfo, holder) then Option.Some(true) else Option.None
                      case _ => Option.None
              else Option.None
          }
        )

    // Shape checks with the semantics of Haskell's `fromBuiltinData` (exact constructor tag, exact
    // number of fields, every field decodes). Each takes the `Data` as its only parameter: with
    // Scalus 1.4.1, a `Data` match in a function of two or more parameters may evaluate
    // `unConstrData` outside the `Constr` branch, failing on a non-constructor instead of
    // returning false.

    /** Whether Haskell's `fromBuiltinData d :: Maybe InsurancePolicyDatum` is a
      * `Just (InsuranceInfo {..})`: constructor 0 with exactly its 15 fields, each of which
      * decodes.
      */
    private def isInsuranceInfo(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            tag === BigInt(0)
            && fields.length === BigInt(15)
            && isInteger(fields.at(0)) // iInfoCollateralAmount
            && isInteger(fields.at(1)) // iInfoFidaCardValue
            && isInteger(fields.at(2)) // iInfoFidaCardNumber
            && isInteger(fields.at(3)) // iInfoPremiumAmount
            && isInstallmentsInfo(fields.at(4)) // iInfoInstallments
            && isInteger(fields.at(5)) // iInfoInsurancePeriod
            && isBytes(fields.at(6)) // iInfoPolicyHolder
            && isInsuranceAuthority(fields.at(7)) // iInfoPolicyAuthority
            && isPolicyState(fields.at(8)) // iInfoState
            && isInteger(fields.at(9)) // iInfoFundingDeadline
            && isInteger(fields.at(10)) // iInfoTotalClaimsAcceptedAmount
            && isInteger(fields.at(11)) // iInfoClaimTimeToLive
            && isInteger(fields.at(12)) // iInfoClaimTimeToPay
            && isMaybeInteger(fields.at(13)) // iInfoStartDate
            && isMaybeClaimInfo(fields.at(14)) // iInfoClaim
        case _ => false

    private def isInteger(d: Data): Boolean = d match
        case Data.I(_) => true
        case _         => false

    private def isBytes(d: Data): Boolean = d match
        case Data.B(_) => true
        case _         => false

    private def isIntegerList(d: Data): Boolean = d match
        case Data.List(elements) => elements.forall(e => isInteger(e))
        case _                   => false

    private def isBytesList(d: Data): Boolean = d match
        case Data.List(elements) => elements.forall(e => isBytes(e))
        case _                   => false

    /** `Bool`: `Constr 0 []` (`False`) or `Constr 1 []` (`True`). */
    private def isBool(d: Data): Boolean = d match
        case Data.Constr(tag, fields) => fields.isEmpty && BigInt(0) <= tag && tag <= BigInt(1)
        case _                        => false

    /** `InsurancePolicyState`: `Constr 0..4 []`. */
    private def isPolicyState(d: Data): Boolean = d match
        case Data.Constr(tag, fields) => fields.isEmpty && BigInt(0) <= tag && tag <= BigInt(4)
        case _                        => false

    /** `Maybe POSIXTime`: `Constr 0 [I _]` (`Just`) or `Constr 1 []` (`Nothing`). */
    private def isMaybeInteger(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            fields match
                case List.Nil                   => tag === BigInt(1)
                case List.Cons(value, List.Nil) => tag === BigInt(0) && isInteger(value)
                case _                          => false
        case _ => false

    /** `Maybe ClaimInfo`: `Constr 0 [ClaimInfo]` (`Just`) or `Constr 1 []` (`Nothing`). */
    private def isMaybeClaimInfo(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            fields match
                case List.Nil                   => tag === BigInt(1)
                case List.Cons(value, List.Nil) => tag === BigInt(0) && isClaimInfo(value)
                case _                          => false
        case _ => false

    /** `InstallmentsInfo [DiffMilliSeconds] PremiumAmount`: `Constr 0 [List [I _, ...], I _]`. */
    private def isInstallmentsInfo(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            if tag === BigInt(0) then
                fields match
                    case List.Cons(intervals, List.Cons(amount, List.Nil)) =>
                        isIntegerList(intervals) && isInteger(amount)
                    case _ => false
            else false
        case _ => false

    /** `InsuranceAuthority`: `Constr 0 [B _]` (`SingleSign`) or `Constr 1..3 [List [B _, ...]]`. */
    private def isInsuranceAuthority(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            fields match
                case List.Cons(keys, List.Nil) =>
                    if tag === BigInt(0) then isBytes(keys)
                    else BigInt(1) <= tag && tag <= BigInt(3) && isBytesList(keys)
                case _ => false
        case _ => false

    /** `ClaimInfo`: `Constr 0 [I _, I _, B _, Bool, B _]`. */
    private def isClaimInfo(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            if tag === BigInt(0) then
                fields match
                    case List.Cons(
                          amount,
                          List.Cons(
                            date,
                            List.Cons(reason, List.Cons(accepted, List.Cons(id, List.Nil)))
                          )
                        ) =>
                        isInteger(amount) && isInteger(date) && isBytes(reason) && isBool(
                          accepted
                        ) && isBytes(id)
                    case _ => false
            else false
        case _ => false
}
