package fida.contract.insurance.lifecycle

import fida.contract.insurance.{InsuranceId, InsurancePolicyDatum, PolicyFundingRedeemer}
import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins
import scalus.cardano.onchain.plutus.v2.ScriptContext

/** Port of `Fida.Contract.Insurance.Lifecycle.Funding` (not implemented yet). */
@Compile
object FundingLifecycle {
    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: PolicyFundingRedeemer,
        ctx: ScriptContext
    ): Boolean = Builtins.trace("NOT-IMPLEMENTED")(false)
}
