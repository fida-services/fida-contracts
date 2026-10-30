package fida.contract

import fida.contract.Utils.{check, txSignedBy}
import scalus.compiler.{Compile, UplcRepr, UplcRepresentation}
import scalus.uplc.builtin.{Builtins, Data, FromData, ToData}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v1.{PolicyId, PubKeyHash, ScriptPurpose}
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.*

/** Identifier of a Fida system deployment. Haskell newtype over `CurrencySymbol`: encoded as a bare
  * bytestring.
  */
@UplcRepr(UplcRepresentation.ProductCaseOneElement)
case class SystemId(cs: PolicyId)

@Compile
object SystemId {
    given ToData[SystemId] = (a: SystemId) => Builtins.bData(a.cs)
    given FromData[SystemId] = (d: Data) => SystemId(Builtins.unBData(d))
}

/** Minting policy of the Fida system identifier (port of `Fida.Contract.SystemId`).
  *
  * Parameters: the system governance key (Haskell `SystemGovernance`, a newtype over `PubKeyHash`)
  * and a magic number (Haskell `SystemMagic`, a newtype over `Integer`) that only serves to derive
  * distinct currency symbols. The redeemer is ignored.
  *
  * On-chain errors:
  *   - `ERR-SID-0`: the transaction is not signed by the system governance key
  *   - `ERR-SID-1`: the script is not run as a minting policy
  */
@Compile
object SystemIdMintingPolicy {

    def validate(governance: PubKeyHash, ctx: ScriptContext): Boolean =
        ctx.purpose match
            case ScriptPurpose.Minting(_) =>
                if txSignedBy(ctx.txInfo, governance) then true
                else Builtins.trace("ERR-SID-0")(false)
            case _ => Builtins.trace("ERR-SID-1")(false)

    def untyped(governance: Data, magic: Data, redeemer: Data, ctx: Data): Unit =
        check(validate(governance.to[PubKeyHash], ctx.to[ScriptContext]))
}
