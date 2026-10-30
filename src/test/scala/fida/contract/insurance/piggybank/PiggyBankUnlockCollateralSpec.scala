package fida.contract.insurance.piggybank

import fida.contract.insurance.{ClaimInfo, PiggyBankDatum}
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Expired, OnRisk}
import fida.contract.insurance.PiggyBankRedeemer.{UnlockCollateral, UnlockCollateralOnCancel}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.Value
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo}
import scalus.cardano.onchain.plutus.prelude.Option

/** Shared transaction shape: the card owner takes the whole collateral back. */
trait UnlockFixtures extends PiggyBankSpecBase {
    val sold: PiggyBankDatum = cardDatum(isSold = true)
    val collateral: Value = value(minAda + fidaCardValue, statusNft)

    def unlock(
        refs: Seq[TxInInfo],
        input: PiggyBankDatum = sold,
        inputValue: Value = collateral,
        owner: Seq[TxInInfo] = Seq(cardOwnerInput)
    ): ScriptContext =
        spendPiggyBank(
          Seq(piggyBankInput(input.toData, inputValue)) ++ owner,
          refs = refs,
          outputs = Seq(txOutNoDatum(investorAddress, inputValue + value(0, cardNft)))
        )
}

/** `UnlockCollateralOnCancel` on any `PBankFidaCard`: ERR-PBANK-25 to 27. */
class PiggyBankUnlockCollateralOnCancelSpec extends UnlockFixtures {

    private val cancelled = sampleInsuranceInfo(state = Cancelled)
    private val refs = Seq(policyInfoRef(cancelled.toData))

    test("UnlockCollateralOnCancel: the card owner takes the collateral of a cancelled policy") {
        assertSucceeds(eval(sold, UnlockCollateralOnCancel, unlock(refs)))
    }

    test("UnlockCollateralOnCancel: other reference datums than InsuranceInfo are ignored") {
        val withPaymentInfo = refs :+ policyInfoRef(samplePremiumPaymentInfo().toData, 2)
        assertSucceeds(eval(sold, UnlockCollateralOnCancel, unlock(withPaymentInfo)))
    }

    test("quirk: an unsold piggy bank holds its own card NFT, so anyone may unlock it") {
        val unsold = cardDatum(isSold = false)
        val fresh = value(minAda, statusNft, cardNft)
        assertSucceeds(
          eval(
            unsold,
            UnlockCollateralOnCancel,
            unlock(refs, input = unsold, inputValue = fresh, owner = Seq.empty)
          )
        )
    }

    test("ERR-PBANK-25: the policy is not cancelled") {
        val onRisk = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(beginningOfTime))
        assertRejectedWith("ERR-PBANK-25")(
          eval(sold, UnlockCollateralOnCancel, unlock(Seq(policyInfoRef(onRisk.toData))))
        )
    }

    test("ERR-PBANK-26: the fida card NFT is not spent") {
        assertRejectedWith("ERR-PBANK-26")(
          eval(sold, UnlockCollateralOnCancel, unlock(refs, owner = Seq.empty))
        )
    }

    test("ERR-PBANK-27.1: no policy info reference input") {
        assertRejectedWith("ERR-PBANK-27.1")(
          eval(sold, UnlockCollateralOnCancel, unlock(Seq.empty))
        )
    }

    test("ERR-PBANK-27.2: two policy info reference inputs") {
        val two = Seq(policyInfoRef(cancelled.toData, 1), policyInfoRef(cancelled.toData, 2))
        assertRejectedWith("ERR-PBANK-27.2")(eval(sold, UnlockCollateralOnCancel, unlock(two)))
    }
}

/** `UnlockCollateral` (expired policy) on any `PBankFidaCard`: ERR-PBANK-28 to 31. */
class PiggyBankUnlockCollateralSpec extends UnlockFixtures {

    private val claim: ClaimInfo = sampleClaim.copy(claimAccepted = true)

    private def expired(claim: Option[ClaimInfo] = Option.None) =
        sampleInsuranceInfo(
          state = Expired,
          startDate = Option.Some(beginningOfTime),
          claim = claim
        )

    private val refs = Seq(policyInfoRef(expired().toData))

    test(
      "UnlockCollateral: the card owner takes the collateral of an expired policy without claim"
    ) {
        assertSucceeds(eval(sold, UnlockCollateral, unlock(refs)))
    }

    test("UnlockCollateral: a claim this piggy bank paid does not block unlocking") {
        val paid = cardDatum(
          isSold = true,
          paidClaims = Seq(ByteString.fromString("other"), claim.claimId)
        )
        val withClaim = Seq(policyInfoRef(expired(Option.Some(claim)).toData))
        assertSucceeds(eval(paid, UnlockCollateral, unlock(withClaim, input = paid)))
    }

    test("ERR-PBANK-28: the policy is not expired") {
        val onRisk = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(beginningOfTime))
        assertRejectedWith("ERR-PBANK-28")(
          eval(sold, UnlockCollateral, unlock(Seq(policyInfoRef(onRisk.toData))))
        )
    }

    test("ERR-PBANK-29: the fida card NFT is not spent") {
        assertRejectedWith("ERR-PBANK-29")(
          eval(sold, UnlockCollateral, unlock(refs, owner = Seq.empty))
        )
    }

    test("ERR-PBANK-30: the policy claim was not paid by this piggy bank") {
        val withClaim = Seq(policyInfoRef(expired(Option.Some(claim)).toData))
        assertRejectedWith("ERR-PBANK-30")(eval(sold, UnlockCollateral, unlock(withClaim)))
    }

    test("ERR-PBANK-31: no policy info reference input") {
        assertRejectedWith("ERR-PBANK-31")(eval(sold, UnlockCollateral, unlock(Seq.empty)))
    }

    test(
      "UnlockCollateral: policy info datums that are not constructors are skipped, not a crash"
    ) {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-31")(
              eval(sold, UnlockCollateral, unlock(Seq(policyInfoRef(d))))
            )
            assertSucceeds(
              eval(
                sold,
                UnlockCollateral,
                unlock(Seq(policyInfoRef(d, 1), policyInfoRef(expired().toData, 2)))
              )
            )
    }

    test("ERR-PBANK-31: two policy info reference inputs") {
        val two = Seq(policyInfoRef(expired().toData, 1), policyInfoRef(expired().toData, 2))
        assertRejectedWith("ERR-PBANK-31")(eval(sold, UnlockCollateral, unlock(two)))
    }
}
