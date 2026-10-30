package fida.contract.insurance.piggybank

import fida.contract.insurance.{ClaimInfo, InsurancePolicyState}
import fida.contract.insurance.InsurancePolicyDatum.PolicyClaimPayment
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Expired, OnRisk}
import fida.contract.insurance.PiggyBankDatum.PBankPremium
import fida.contract.insurance.PiggyBankRedeemer.*
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.Interval
import scalus.cardano.onchain.plutus.prelude.Option

/** When several checks fail, the reported error follows the Haskell evaluation order: conjuncts
  * left to right, each `where` binding evaluated when the first conjunct needing it runs, and the
  * left operand of a comparison before the right one.
  */
class PiggyBankEvaluationOrderSpec extends PiggyBankSpecBase {

    private val sold = cardDatum(isSold = true)
    private val premium = PBankPremium(20_000_000, 0)
    private val acceptedClaim = sampleClaim.copy(claimAccepted = true)

    private def policy(state: InsurancePolicyState, claim: Option[ClaimInfo] = Option.None) =
        sampleInsuranceInfo(state = state, startDate = Option.Some(beginningOfTime), claim = claim)

    test("SellFidaCard: without own input, getContinuingOutputs fails with Lf before ERR-PBANK-8") {
        val info = txInfo(inputs = Seq(piggyBankInput(sold.toData, value(minAda, statusNft))))
        assertFailsWithoutErrorCode(eval(sold, SellFidaCard, minting(info, cs)), Some("Lf"))
    }

    test("ClaimPremium: ERR-PBANK-7 comes before the policy info lookup (ERR-PBANK-9)") {
        val ctx = spendPiggyBank(Seq(piggyBankInput(premium.toData, value(20_000_000))))
        assertRejectedWith("ERR-PBANK-7")(eval(premium, ClaimPremium, ctx))
    }

    test(
      "ClaimPremium: the locked premium (Lf without own input) is evaluated before ERR-PBANK-9"
    ) {
        val info =
            txInfo(inputs = Seq(piggyBankInput(premium.toData, value(20_000_000)), cardOwnerInput))
        assertFailsWithoutErrorCode(eval(premium, ClaimPremium, minting(info, cs)), Some("Lf"))
    }

    test("ClaimPremium: ERR-PBANK-9 comes before ERR-PBANK-10 and the amount check") {
        val refs = Seq(
          policyInfoRef(sampleInsuranceInfo(state = OnRisk).toData, 1),
          policyInfoRef(sampleInsuranceInfo(state = OnRisk).toData, 2)
        )
        val ctx = spendPiggyBank(
          Seq(piggyBankInput(premium.toData, value(20_000_000)), cardOwnerInput),
          refs = refs
        )
        assertRejectedWith("ERR-PBANK-9.2")(eval(premium, ClaimPremium, ctx))
    }

    test(
      "PayForClaimWithCollateral: ERR-PBANK-18 and ERR-PBANK-11 come before the own input (ERR-PBANK-19)"
    ) {
        val noClaim = txInfo(
          inputs = Seq(piggyBankInput(sold.toData, value(minAda, statusNft))),
          referenceInputs = Seq(policyInfoRef(policy(OnRisk).toData))
        )
        assertRejectedWith("ERR-PBANK-18")(
          eval(sold, PayForClaimWithCollateral, minting(noClaim, cs))
        )
        val notAccepted = txInfo(
          inputs = Seq(piggyBankInput(sold.toData, value(minAda, statusNft))),
          referenceInputs = Seq(policyInfoRef(policy(OnRisk, Option.Some(sampleClaim)).toData))
        )
        assertRejectedWith("ERR-PBANK-11")(
          eval(sold, PayForClaimWithCollateral, minting(notAccepted, cs))
        )
    }

    test("PayForClaimWithCollateral: ERR-PBANK-19 comes before the status output (ERR-PBANK-20)") {
        val info = txInfo(
          inputs = Seq(piggyBankInput(sold.toData, value(minAda, statusNft))),
          referenceInputs = Seq(policyInfoRef(policy(OnRisk, Option.Some(acceptedClaim)).toData))
        )
        assertRejectedWith("ERR-PBANK-19")(eval(sold, PayForClaimWithCollateral, minting(info, cs)))
    }

    test("PayForClaimWithCollateral: ERR-PBANK-20 comes before every amount and claim check") {
        val ctx = spendPiggyBank(
          Seq(piggyBankInput(sold.toData, value(minAda + fidaCardValue, statusNft))),
          refs = Seq(policyInfoRef(policy(OnRisk, Option.Some(acceptedClaim)).toData))
        )
        assertRejectedWith("ERR-PBANK-20")(eval(sold, PayForClaimWithCollateral, ctx))
    }

    test("PayForClaimWithCollateral: ERR-PBANK-12 comes before ERR-PBANK-13 to 17") {
        val claimed = cardDatum(isSold = true, paidClaims = Seq(acceptedClaim.claimId))
        val wrongOutput = cardDatum(isSold = true, cardValue = 1)
        val ctx = spendPiggyBank(
          Seq(piggyBankInput(claimed.toData, value(minAda + fidaCardValue, statusNft))),
          refs = Seq(policyInfoRef(policy(OnRisk, Option.Some(acceptedClaim)).toData)),
          outputs = Seq(piggyBankOutput(wrongOutput.toData, value(minAda, statusNft)))
        )
        assertRejectedWith("ERR-PBANK-12")(eval(claimed, PayForClaimWithCollateral, ctx))
    }

    test("PayForClaimWithCollateral: ERR-PBANK-15 comes before ERR-PBANK-16 and 17") {
        val share = acceptedClaim.claimAmount / fidaCardQuantity
        val wrongValue =
            cardDatum(isSold = true, cardValue = 1, paidClaims = Seq(acceptedClaim.claimId))
        val ctx = spendPiggyBank(
          Seq(piggyBankInput(sold.toData, value(minAda + fidaCardValue, statusNft))),
          refs = Seq(policyInfoRef(policy(OnRisk, Option.Some(acceptedClaim)).toData)),
          outputs = Seq(
            piggyBankOutput(wrongValue.toData, value(minAda + fidaCardValue - share, statusNft)),
            txOut(policyAddress, value(share - 1), PolicyClaimPayment.toData)
          )
        )
        assertRejectedWith("ERR-PBANK-15")(eval(sold, PayForClaimWithCollateral, ctx))
    }

    test("ClaimPremiumOnCancel: ERR-PBANK-24 comes first, then ERR-PBANK-21, 22, 23 and 32") {
        val noRefs = spendPiggyBank(Seq(piggyBankInput(premium.toData, value(20_000_000))))
        assertRejectedWith("ERR-PBANK-24.1")(eval(premium, ClaimPremiumOnCancel, noRefs))
        val refunded = PBankPremium(20_000_000, 1)
        def ctx(state: InsurancePolicyState) = spendPiggyBank(
          Seq(piggyBankInput(refunded.toData, value(20_000_000))),
          refs = Seq(policyInfoRef(policy(state).toData)),
          outputs = Seq(txOutNoDatum(investorAddress, value(20_000_000))),
          validRange = Interval.after(beginningOfTime + days(90) + 1)
        )
        // not cancelled, not signed, too much taken and already refunded: ERR-PBANK-21
        assertRejectedWith("ERR-PBANK-21")(eval(refunded, ClaimPremiumOnCancel, ctx(OnRisk)))
        // cancelled: ERR-PBANK-22
        assertRejectedWith("ERR-PBANK-22")(eval(refunded, ClaimPremiumOnCancel, ctx(Cancelled)))
    }

    test(
      "UnlockCollateralOnCancel and UnlockCollateral: the policy state comes before card ownership"
    ) {
        val ctx = (state: InsurancePolicyState) =>
            spendPiggyBank(
              Seq(piggyBankInput(sold.toData, value(minAda, statusNft))),
              refs = Seq(policyInfoRef(policy(state).toData))
            )
        assertRejectedWith("ERR-PBANK-25")(eval(sold, UnlockCollateralOnCancel, ctx(Expired)))
        assertRejectedWith("ERR-PBANK-28")(eval(sold, UnlockCollateral, ctx(Cancelled)))
    }
}
