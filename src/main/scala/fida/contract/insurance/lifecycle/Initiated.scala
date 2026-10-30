package fida.contract.insurance.lifecycle

import fida.contract.insurance.{InsuranceId, InsurancePolicyDatum, PolicyInitiatedRedeemer}
import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins
import scalus.cardano.onchain.plutus.v2.ScriptContext

/** Port of `Fida.Contract.Insurance.Lifecycle.Initiated` (not implemented yet). */
@Compile
object InitiatedLifecycle {
    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: PolicyInitiatedRedeemer,
        ctx: ScriptContext
    ): Boolean = Builtins.trace("NOT-IMPLEMENTED")(false)
}
