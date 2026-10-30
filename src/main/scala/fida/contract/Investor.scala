package fida.contract

import scalus.compiler.Compile
import scalus.uplc.builtin.{Data, FromData, ToData}

/** Investor redeemer: `ProcessClaim` 0, `Withdraw` 1. */
enum InvestorRedeemer:
    case ProcessClaim, Withdraw

@Compile
object InvestorRedeemer {
    given ToData[InvestorRedeemer] = ToData.derived
    given FromData[InvestorRedeemer] = FromData.derived
}

/** Investor validator (port of `Fida.Contract.Investor`).
  *
  * Parameters: the investor's `PubKeyHash` and the `SystemId`. The Haskell implementation is a
  * placeholder that accepts every spend (`mkInvestorValidator _ _ _ _ _ = True`); this port keeps
  * that behaviour.
  */
@Compile
object InvestorValidator {
    def untyped(investor: Data, systemId: Data, datum: Data, redeemer: Data, ctx: Data): Unit = ()
}
