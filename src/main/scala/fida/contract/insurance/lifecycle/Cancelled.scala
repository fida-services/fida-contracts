package fida.contract.insurance.lifecycle

import fida.contract.insurance.{InsuranceId, InsurancePolicyDatum, InsurancePolicyRedeemer}
import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins
import scalus.cardano.onchain.plutus.v2.ScriptContext

/** A cancelled policy accepts no further actions (port of
  * `Fida.Contract.Insurance.Lifecycle.Cancelled`).
  *
  * On-chain errors:
  *   - `ERR-CANC-0`: no action is allowed for a cancelled policy
  */
@Compile
object CancelledLifecycle {
    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: InsurancePolicyRedeemer,
        ctx: ScriptContext
    ): Boolean = Builtins.trace("ERR-CANC-0")(false)
}
