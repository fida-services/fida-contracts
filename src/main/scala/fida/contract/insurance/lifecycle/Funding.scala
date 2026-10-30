package fida.contract.insurance.lifecycle

import fida.contract.Utils.{before, traceIfFalse, txSignedBy, untypedOutputDatum, valueOf}
import fida.contract.insurance.{InsuranceId, InsurancePolicyDatum, PolicyFundingRedeemer, Tokens}
import fida.contract.insurance.InsuranceAuthority.isSignedByTheAuthority
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, completeFunding, untypedUpdatePolicyState}
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Funding}
import fida.contract.insurance.PolicyFundingRedeemer.{PolicyFundingCancel, PolicyFundingFundingComplete}
import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.PolicyId
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInfo}
import scalus.cardano.onchain.plutus.prelude.*

/** Funding policy actions (port of `Fida.Contract.Insurance.Lifecycle.Funding`).
  *
  *   - Cancel (`InsuranceInfo` in any state, as in the Haskell code): signed by the policy
  *     authority, the policy info moves to Cancelled;
  *   - FundingComplete `startDate` (`InsuranceInfo` in state Funding): every fida card is sold, the
  *     policy info moves to OnRisk with `startDate`, which lies before the validity range, signed by
  *     the policy holder or the policy authority. Sold fida cards are the distinct ids of the
  *     reference inputs holding exactly one CARD-STATUS token whose inline datum decodes to a sold
  *     `PBankFidaCard`.
  *
  * On-chain errors:
  *   - `ERR-FUND-0`: illegal action for the Funding state
  *   - `ERR-FUND-1`: the transaction is not signed by the policy authority
  *   - `ERR-FUND-2`: the output datum is not updated correctly
  *   - `ERR-FUND-3`: the number of sold fida cards is smaller than the number of fida cards
  *   - `ERR-FUND-4`: the output datum is not updated correctly
  *   - `ERR-FUND-5`: the start date must be before the validity range
  *   - `ERR-FUND-6`: the transaction is not signed by the policy holder or the policy authority
  */
@Compile
object FundingLifecycle {

    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: PolicyFundingRedeemer,
        ctx: ScriptContext
    ): Boolean =
        redeemer match
            case PolicyFundingCancel =>
                datum match
                    case InsuranceInfo(_, _, _, _, _, _, _, authority, _, _, _, _, _, _, _) =>
                        traceIfFalse("ERR-FUND-1", isSignedByTheAuthority(ctx, authority))
                        && traceIfFalse(
                          "ERR-FUND-2",
                          untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)
                              === untypedUpdatePolicyState(datum, Cancelled)
                        )
                    case _ => illegalAction
            case PolicyFundingFundingComplete(startDate) =>
                datum match
                    case InsuranceInfo(_, _, fidaCardNumber, _, _, _, policyHolder, authority, state, _, _, _, _, _, _) =>
                        if state === Funding then
                            val txInfo = ctx.txInfo
                            traceIfFalse("ERR-FUND-3", fidaCardsSold(iid.cs, txInfo) >= fidaCardNumber)
                            && traceIfFalse(
                              "ERR-FUND-4",
                              untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)
                                  === completeFunding(datum, startDate).map(_.toData)
                            )
                            && traceIfFalse("ERR-FUND-5", before(startDate, txInfo.validRange))
                            && traceIfFalse(
                              "ERR-FUND-6",
                              txSignedBy(txInfo, policyHolder) || isSignedByTheAuthority(ctx, authority)
                            )
                        else illegalAction
                    case _ => illegalAction

    /** Haskell `trace "ERR-FUND-0" False`. */
    private inline def illegalAction: Boolean = Builtins.trace("ERR-FUND-0")(false)

    /** Haskell `fidaCardsSold`: the number of distinct fida card ids among the reference inputs that
      * hold exactly one CARD-STATUS token and whose inline datum decodes to
      * `PBankFidaCard {pbfcIsSold = True}`.
      */
    private def fidaCardsSold(cs: PolicyId, txInfo: TxInfo): BigInt =
        txInfo.referenceInputs
            .filterMap { in =>
                in.resolved.datum match
                    case OutputDatum.OutputDatum(d) =>
                        if valueOf(in.resolved.value, cs, Tokens.fidaCardStatusTokenName) === BigInt(1)
                        then soldFidaCardId(d)
                        else Option.None
                    case _ => Option.None
            }
            .distinct
            .length

    /** The fida card id when Haskell's `fromBuiltinData d :: Maybe PiggyBankDatum` is a
      * `Just (PBankFidaCard {pbfcIsSold = True, pbfcFidaCardId = FidaCardId cid})`, i.e. `d` is
      * exactly `Constr 1 [Constr 1 [], B cid, I _, List [B _, ...]]`.
      */
    private def soldFidaCardId(d: Data): Option[ByteString] = d match
        case Data.Constr(tag, fields) =>
            if tag === BigInt(1) then
                fields match
                    case List.Cons(isSold, List.Cons(cardId, List.Cons(cardValue, List.Cons(paidClaims, List.Nil)))) =>
                        cardId match
                            case Data.B(cid) =>
                                if isTrue(isSold) && isInteger(cardValue) && isByteStringList(paidClaims)
                                then Option.Some(cid)
                                else Option.None
                            case _ => Option.None
                    case _ => Option.None
            else Option.None
        case _ => Option.None

    /** `True`: exactly `Constr 1 []`. */
    private def isTrue(d: Data): Boolean = d match
        case Data.Constr(tag, fields) => tag === BigInt(1) && fields.isEmpty
        case _                        => false

    private def isInteger(d: Data): Boolean = d match
        case Data.I(_) => true
        case _         => false

    private def isByteStringList(d: Data): Boolean = d match
        case Data.List(elements) =>
            elements.forall { e =>
                e match
                    case Data.B(_) => true
                    case _         => false
            }
        case _ => false
}
