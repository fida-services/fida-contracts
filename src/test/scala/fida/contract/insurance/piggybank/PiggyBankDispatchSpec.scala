package fida.contract.insurance.piggybank

import fida.contract.insurance.{PiggyBankDatum, PiggyBankRedeemer}
import fida.contract.insurance.PiggyBankDatum.PBankPremium
import fida.contract.insurance.PiggyBankRedeemer.*
import fida.contract.testkit.ContextBuilder.*
import scalus.uplc.builtin.Data.toData

/** Dispatch on redeemer and datum: every combination without a Haskell clause fails with
  * `traceError "ERR-PBANK-32"`.
  */
class PiggyBankDispatchSpec extends PiggyBankSpecBase {

    private val unsold = cardDatum(isSold = false)
    private val sold = cardDatum(isSold = true)
    private val premium = PBankPremium(20_000_000, 0)

    private val notAllowed: Seq[(PiggyBankRedeemer, String, PiggyBankDatum)] = Seq(
      (BuyFidaCard, "a sold card", sold),
      (BuyFidaCard, "a premium", premium),
      (SellFidaCard, "an unsold card", unsold),
      (SellFidaCard, "a premium", premium),
      (ClaimPremium, "an unsold card", unsold),
      (ClaimPremium, "a sold card", sold),
      (PayForClaimWithCollateral, "an unsold card", unsold),
      (PayForClaimWithCollateral, "a premium", premium),
      (ClaimPremiumOnCancel, "an unsold card", unsold),
      (ClaimPremiumOnCancel, "a sold card", sold),
      (UnlockCollateralOnCancel, "a premium", premium),
      (UnlockCollateral, "a premium", premium)
    )

    for (redeemer, what, datum) <- notAllowed do
        test(s"ERR-PBANK-32: $redeemer is not allowed on $what") {
            val ctx =
                spendPiggyBank(Seq(piggyBankInput(datum.toData, value(minAda)), cardOwnerInput))
            val result = eval(datum, redeemer, ctx)
            assertRejectedWith("ERR-PBANK-32")(result)
            assert(!result.logs.contains("PT5"), "expected traceError, not a false result")
        }
}
