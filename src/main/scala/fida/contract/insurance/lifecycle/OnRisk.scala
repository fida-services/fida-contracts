package fida.contract.insurance.lifecycle

import fida.contract.insurance.{InsuranceId, InsurancePolicyDatum, PolicyOnRiskRedeemer}
import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins
import scalus.cardano.onchain.plutus.v2.ScriptContext

/** Port of `Fida.Contract.Insurance.Lifecycle.OnRisk` (not implemented yet). */
@Compile
object OnRiskLifecycle {
    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: PolicyOnRiskRedeemer,
        ctx: ScriptContext
    ): Boolean = Builtins.trace("NOT-IMPLEMENTED")(false)
}
