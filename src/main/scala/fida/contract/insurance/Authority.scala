package fida.contract.insurance

import fida.contract.Utils.txSignedBy
import scalus.compiler.Compile
import scalus.uplc.builtin.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.*

/** Who may authorise privileged policy actions (port of `Fida.Contract.Insurance.Authority`).
  *
  * On-chain encoding (`makeIsDataIndexed`): `SingleSign` 0, `AtLeastOneSign` 1, `AllMustSign` 2,
  * `MajorityMustSign` 3.
  */
enum InsuranceAuthority:
    case SingleSign(pkh: PubKeyHash)
    case AtLeastOneSign(pkhs: List[PubKeyHash])
    case AllMustSign(pkhs: List[PubKeyHash])
    case MajorityMustSign(pkhs: List[PubKeyHash])

@Compile
object InsuranceAuthority {
    given ToData[InsuranceAuthority] = ToData.derived
    given FromData[InsuranceAuthority] = FromData.derived

    /** Whether `ctx`'s transaction satisfies the authority's signing strategy.
      *
      * Mirrors the Haskell implementation exactly, including that `AtLeastOneSign` is satisfied
      * only when exactly one of the listed keys signs, and that duplicated keys are counted once
      * per occurrence.
      */
    def isSignedByTheAuthority(ctx: ScriptContext, auth: InsuranceAuthority): Boolean =
        val txInfo = ctx.txInfo
        def signedBy(pkhs: List[PubKeyHash]): BigInt = pkhs.count(pkh => txSignedBy(txInfo, pkh))
        auth match
            case SingleSign(pkh)        => txSignedBy(txInfo, pkh)
            case AtLeastOneSign(pkhs)   => signedBy(pkhs) === BigInt(1)
            case AllMustSign(pkhs)      => signedBy(pkhs) === pkhs.length
            case MajorityMustSign(pkhs) => BigInt(2) * signedBy(pkhs) > pkhs.length
}
