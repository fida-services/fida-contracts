package fida.contract.insurance.lifecycle

import fida.contract.Utils.{
    getContinuingOutputs,
    lovelaceValueOf,
    traceIfFalse,
    traceIfNotSingleton,
    untypedOutputDatum,
    valueOf
}
import fida.contract.insurance.{
    InsuranceId,
    InsurancePolicyDatum,
    PiggyBankDatum,
    PolicyInitiatedRedeemer,
    Tokens
}
import fida.contract.insurance.InsuranceAuthority.isSignedByTheAuthority
import fida.contract.insurance.InsurancePolicyDatum.{
    InsuranceInfo,
    PremiumPaymentInfo,
    untypedUpdatePolicyState
}
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Funding, Initiated}
import fida.contract.insurance.PiggyBankDatum.PBankPremium
import fida.contract.insurance.PolicyInitiatedRedeemer.{
    PolicyInitiatedCancel,
    PolicyInitiatedPayPremium
}
import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, Data}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v1.{Address, PolicyId, TokenName, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxOut}
import scalus.cardano.onchain.plutus.prelude.*

/** Initiated policy actions (port of `Fida.Contract.Insurance.Lifecycle.Initiated`).
  *
  * At the script address an initiated policy has two UTxOs: the policy info (`InsuranceInfo` in
  * state Initiated, marked by the POLICY-INFO NFT) and the premium payment info
  * (`PremiumPaymentInfo`, marked by the POLICY-PAYMENT NFT). The actions are:
  *   - Cancel (`InsuranceInfo` in state Initiated): signed by the policy authority, the policy info
  *     moves to Cancelled;
  *   - PayPremium, for the premium payment info: every listed piggy bank receives its premium and
  *     the policy info is recreated;
  *   - PayPremium, for the policy info (in any state, as in the Haskell code): it moves to Funding
  *     and a POLICY-PAYMENT token is spent.
  *
  * On-chain errors:
  *   - `ERR-INIT-0`: illegal action for the Initiated state
  *   - `ERR-INIT-1`: transaction not signed by the policy authority
  *   - `ERR-INIT-2`: insurance policy state not changed to Cancelled
  *   - `ERR-INIT-3`: payments not properly distributed
  *   - `ERR-INIT-4`: UTxO with policy datum was not spent
  *   - `ERR-INIT-5`: documented by the Haskell code (payment info not marked by NFT) but never
  *     raised
  *   - `ERR-INIT-6`: insurance policy state not changed to Funding
  *   - `ERR-INIT-7`: UTxO with premium payment was not spent
  */
@Compile
object InitiatedLifecycle {

    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: PolicyInitiatedRedeemer,
        ctx: ScriptContext
    ): Boolean =
        redeemer match
            case PolicyInitiatedCancel =>
                datum match
                    case InsuranceInfo(_, _, _, _, _, _, _, authority, state, _, _, _, _, _, _) =>
                        if state === Initiated then
                            traceIfFalse("ERR-INIT-1", isSignedByTheAuthority(ctx, authority))
                            && traceIfFalse(
                              "ERR-INIT-2",
                              untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)
                                  === untypedUpdatePolicyState(datum, Cancelled)
                            )
                        else illegalAction
                    case _ => illegalAction
            case PolicyInitiatedPayPremium =>
                datum match
                    case PremiumPaymentInfo(premiumPerPiggyBank, piggyBanks) =>
                        val payments = ctx.txInfo.outputs.filterMap(out =>
                            premiumPayment(out, premiumPerPiggyBank, piggyBanks)
                        )
                        traceIfFalse("ERR-INIT-3", payments.distinct.length === piggyBanks.length)
                        && traceIfNotSingleton(
                          "ERR-INIT-4",
                          getContinuingOutputs(ctx).filterMap(out =>
                              holdsOne(out.value, iid.cs, Tokens.policyInfoTokenName)
                          )
                        )
                    case InsuranceInfo(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                        traceIfFalse(
                          "ERR-INIT-6",
                          untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)
                              === untypedUpdatePolicyState(datum, Funding)
                        )
                        && traceIfNotSingleton(
                          "ERR-INIT-7",
                          ctx.txInfo.inputs.filterMap(in =>
                              holdsOne(in.resolved.value, iid.cs, Tokens.policyPaymentTokenName)
                          )
                        )
                    case _ => illegalAction

    /** Haskell `trace "ERR-INIT-0" False`. */
    private inline def illegalAction: Boolean = Builtins.trace("ERR-INIT-0")(false)

    /** An element of the Haskell `[True | ..., valueOf value cs tn == 1]` comprehensions. */
    private def holdsOne(v: Value, cs: PolicyId, tn: TokenName): Option[Boolean] =
        if valueOf(v, cs, tn) === BigInt(1) then Option.Some(true) else Option.None

    /** One element of the Haskell `payments` comprehension: the address of `out` when its inline
      * datum decodes to `PBankPremium amount refund`, it pays one of `piggyBanks`, `refund == 0`
      * and the lovelace paid is at least `premiumPerPiggyBank` and equal to `amount`.
      */
    private def premiumPayment(
        out: TxOut,
        premiumPerPiggyBank: BigInt,
        piggyBanks: List[Address]
    ): Option[Address] =
        out.datum match
            case OutputDatum.OutputDatum(d) =>
                if isPBankPremium(d) then
                    d.to[PiggyBankDatum] match
                        case PBankPremium(amount, refund) =>
                            val paid = lovelaceValueOf(out.value)
                            if piggyBanks.contains(out.address)
                                && refund === BigInt(0)
                                && paid >= premiumPerPiggyBank
                                && paid === amount
                            then Option.Some(out.address)
                            else Option.None
                        case _ => Option.None
                else Option.None
            case _ => Option.None

    /** Whether Haskell's `fromBuiltinData d :: Maybe PiggyBankDatum` is a
      * `Just (PBankPremium _ _)`, i.e. `d` is exactly `Constr 0 [I _, I _]`.
      */
    private def isPBankPremium(d: Data): Boolean = d match
        case Data.Constr(tag, fields) =>
            if tag === BigInt(0) then
                fields match
                    case List.Cons(amount, List.Cons(refund, List.Nil)) =>
                        isInteger(amount) && isInteger(refund)
                    case _ => false
            else false
        case _ => false

    private def isInteger(d: Data): Boolean = d match
        case Data.I(_) => true
        case _         => false
}
