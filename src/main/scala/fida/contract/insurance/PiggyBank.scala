package fida.contract.insurance

import fida.contract.Utils.check
import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, Data}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v2.ScriptContext

/** Piggy bank validator (port of `Fida.Contract.Insurance.PiggyBank`, not implemented yet).
  *
  * Parameters: the [[InsuranceId]] and the [[FidaCardId]] of the fida card the piggy bank backs.
  */
@Compile
object PiggyBankValidator {

    def validate(
        iid: InsuranceId,
        fcid: FidaCardId,
        datum: PiggyBankDatum,
        redeemer: PiggyBankRedeemer,
        ctx: ScriptContext
    ): Boolean = Builtins.trace("NOT-IMPLEMENTED")(false)

    def untyped(iid: Data, fcid: Data, datum: Data, redeemer: Data, ctx: Data): Unit =
        check(
          validate(
            iid.to[InsuranceId],
            fcid.to[FidaCardId],
            datum.to[PiggyBankDatum],
            redeemer.to[PiggyBankRedeemer],
            ctx.to[ScriptContext]
          )
        )
}
