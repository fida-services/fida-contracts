package fida.contract.insurance

import fida.contract.Utils.{before, check, traceError, traceIfFalse, untypedOutputDatum}
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, untypedUpdatePolicyState}
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Expired, Funding, OnRisk}
import fida.contract.insurance.lifecycle.{CancelledLifecycle, FundingLifecycle, InitiatedLifecycle, OnRiskLifecycle}
import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, Data}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.*

/** Insurance policy validator (port of `Fida.Contract.Insurance`).
  *
  * Parameter: the [[InsuranceId]]. Dispatches on the redeemer and datum in the same order as the
  * Haskell pattern match:
  *   1. `PolicyInitiated r` (any datum) is handled by [[InitiatedLifecycle]];
  *   2. any other redeemer on an `InsuranceInfo` datum in state `Cancelled` is handled by
  *      [[CancelledLifecycle]] (always rejected);
  *   3. `PolicyFunding r` by [[FundingLifecycle]], `PolicyOnRisk r` by [[OnRiskLifecycle]];
  *   4. `PolicyExpire` on an `InsuranceInfo` datum moves a policy to `Expired`.
  *
  * On-chain errors:
  *   - `ERR-MAIN-0`: illegal action for the main validator
  *   - `ERR-MAIN-1`: expiry requested for a policy that is neither `Funding` nor `OnRisk` with a
  *     start date
  *   - `ERR-MAIN-2`: the policy has not expired yet
  *   - `ERR-MAIN-3`: the policy info output datum is not the input datum in state `Expired`
  */
@Compile
object InsurancePolicyValidator {

    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: InsurancePolicyRedeemer,
        ctx: ScriptContext
    ): Boolean =
        redeemer match
            case PolicyInitiated(r) => InitiatedLifecycle.validate(iid, datum, r, ctx)
            case _ =>
                if isCancelled(datum) then CancelledLifecycle.validate(iid, datum, redeemer, ctx)
                else
                    redeemer match
                        case PolicyFunding(r) => FundingLifecycle.validate(iid, datum, r, ctx)
                        case PolicyOnRisk(r)  => OnRiskLifecycle.validate(iid, datum, r, ctx)
                        case PolicyExpire     => validateExpire(iid, datum, ctx)
                        case _                => Builtins.trace("ERR-MAIN-0")(false)

    private def isCancelled(datum: InsurancePolicyDatum): Boolean = datum match
        case InsuranceInfo(_, _, _, _, _, _, _, _, state, _, _, _, _, _, _) => state === Cancelled
        case _                                                              => false

    private def validateExpire(iid: InsuranceId, datum: InsurancePolicyDatum, ctx: ScriptContext): Boolean =
        datum match
            case InsuranceInfo(_, _, _, _, _, insurancePeriod, _, _, state, fundingDeadline, _, _, _, startDate, _) =>
                val expireDate =
                    if state === Funding then fundingDeadline
                    else if state === OnRisk then
                        startDate match
                            case Option.Some(start) => start + insurancePeriod
                            case Option.None        => traceError("ERR-MAIN-1")
                    else traceError("ERR-MAIN-1")
                traceIfFalse("ERR-MAIN-2", before(expireDate, ctx.txInfo.validRange))
                && traceIfFalse(
                  "ERR-MAIN-3",
                  untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)
                      === untypedUpdatePolicyState(datum, Expired)
                )
            case _ => Builtins.trace("ERR-MAIN-0")(false)

    def untyped(iid: Data, datum: Data, redeemer: Data, ctx: Data): Unit =
        check(
          validate(
            iid.to[InsuranceId],
            datum.to[InsurancePolicyDatum],
            redeemer.to[InsurancePolicyRedeemer],
            ctx.to[ScriptContext]
          )
        )
}
