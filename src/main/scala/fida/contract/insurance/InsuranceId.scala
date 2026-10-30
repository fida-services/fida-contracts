package fida.contract.insurance

import fida.contract.Utils.check
import scalus.compiler.{Compile, UplcRepr, UplcRepresentation}
import scalus.uplc.builtin.{Builtins, Data, FromData, ToData}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v1.{PolicyId, ScriptPurpose, TxOutRef}
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.*

/** Unique Fida insurance identifier: the currency symbol of the insurance's
  * [[InsuranceIdMintingPolicy]]. Haskell newtype over `CurrencySymbol`: encoded as a bare
  * bytestring.
  */
@UplcRepr(UplcRepresentation.ProductCaseOneElement)
case class InsuranceId(cs: PolicyId)

@Compile
object InsuranceId {
    given Eq[InsuranceId] = Eq.derived
    given ToData[InsuranceId] = (a: InsuranceId) => Builtins.bData(a.cs)
    given FromData[InsuranceId] = (d: Data) => InsuranceId(Builtins.unBData(d))
}

/** One-shot minting policy that defines an insurance's currency symbol (port of
  * `Fida.Contract.Insurance.InsuranceId`).
  *
  * Parameter: the `TxOutRef` that must be spent by the minting transaction, which makes the
  * currency symbol unique. The redeemer is ignored.
  *
  * On-chain errors:
  *   - `ERR-IID-0`: the parameter `TxOutRef` is not spent by the transaction
  *   - `ERR-IID-1`: the script is not run as a minting policy
  */
@Compile
object InsuranceIdMintingPolicy {

    def validate(ref: TxOutRef, ctx: ScriptContext): Boolean =
        ctx.purpose match
            case ScriptPurpose.Minting(_) =>
                if ctx.txInfo.inputs.exists(_.outRef === ref) then true
                else Builtins.trace("ERR-IID-0")(false)
            case _ => Builtins.trace("ERR-IID-1")(false)

    def untyped(ref: Data, redeemer: Data, ctx: Data): Unit =
        check(validate(ref.to[TxOutRef], ctx.to[ScriptContext]))
}
