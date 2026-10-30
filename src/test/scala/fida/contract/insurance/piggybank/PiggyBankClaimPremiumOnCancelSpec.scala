package fida.contract.insurance.piggybank

import fida.contract.insurance.PiggyBankDatum
import fida.contract.insurance.InsurancePolicyState.{Cancelled, OnRisk}
import fida.contract.insurance.PiggyBankDatum.PBankPremium
import fida.contract.insurance.PiggyBankRedeemer.ClaimPremiumOnCancel
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Interval, PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo}
import scalus.cardano.onchain.plutus.prelude.Option

/** `ClaimPremiumOnCancel` (refund to the policy holder) on a `PBankPremium`: ERR-PBANK-21 to 24 and
  * the fourth check, which reuses ERR-PBANK-32.
  */
class PiggyBankClaimPremiumOnCancelSpec extends PiggyBankSpecBase {

    /** Premium paid into each piggy bank: 20 ADA, unlocked to investors 5 ADA per installment. */
    private val share: BigInt = policyPrice / fidaCardQuantity
    private val installment: BigInt = share / 4

    private val premium = PBankPremium(share, 0)
    private val start: PosixTime = beginningOfTime
    private val cancelled = sampleInsuranceInfo(state = Cancelled, startDate = Option.Some(start))
    private val afterFirstInstallment = Interval.after(start + days(90) + 1)

    /** Refund due after the first installment: what investors have not earned yet (15 ADA). */
    private val refund: BigInt = share - installment

    /** The policy holder takes `taken`; the rest stays in a continuing output with `lockedDatum`.
      */
    private def refundTx(
        taken: BigInt = refund,
        input: PiggyBankDatum = premium,
        inputLovelace: BigInt = share,
        lockedDatum: Data = PBankPremium(share, refund).toData,
        refs: Seq[TxInInfo] = Seq(policyInfoRef(cancelled.toData)),
        signers: Seq[PubKeyHash] = Seq(policyHolder),
        validRange: Interval = afterFirstInstallment
    ): ScriptContext =
        val locked = inputLovelace - taken
        val lockedOutput =
            if locked > 0 then Seq(piggyBankOutput(lockedDatum, value(locked))) else Seq.empty
        spendPiggyBank(
          Seq(piggyBankInput(input.toData, value(inputLovelace))),
          refs = refs,
          outputs = lockedOutput :+ txOutNoDatum(pubKeyAddress(policyHolder), value(taken)),
          validRange = validRange,
          signatories = signers
        )

    test(
      "ClaimPremiumOnCancel: the policy holder takes back the premium investors have not earned"
    ) {
        assertSucceeds(eval(premium, ClaimPremiumOnCancel, refundTx()))
    }

    test("ClaimPremiumOnCancel: the policy holder may take less than the refund") {
        assertSucceeds(eval(premium, ClaimPremiumOnCancel, refundTx(taken = refund - 1)))
    }

    test("ClaimPremiumOnCancel: without a start date the whole premium is refunded") {
        val neverStarted = sampleInsuranceInfo(state = Cancelled)
        assertSucceeds(
          eval(
            premium,
            ClaimPremiumOnCancel,
            refundTx(taken = share, refs = Seq(policyInfoRef(neverStarted.toData)))
          )
        )
    }

    test("ERR-PBANK-21: the policy is not cancelled") {
        val onRisk = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(start))
        assertRejectedWith("ERR-PBANK-21")(
          eval(premium, ClaimPremiumOnCancel, refundTx(refs = Seq(policyInfoRef(onRisk.toData))))
        )
    }

    test("ERR-PBANK-22: the policy holder does not sign") {
        assertRejectedWith("ERR-PBANK-22")(
          eval(premium, ClaimPremiumOnCancel, refundTx(signers = Seq(investor1)))
        )
    }

    test("ERR-PBANK-23: taking more than the refund amount") {
        assertRejectedWith("ERR-PBANK-23")(
          eval(premium, ClaimPremiumOnCancel, refundTx(taken = refund + 1))
        )
    }

    test("ERR-PBANK-23: the continuing output must record the refund amount") {
        assertRejectedWith("ERR-PBANK-23")(
          eval(premium, ClaimPremiumOnCancel, refundTx(lockedDatum = premium.toData))
        )
    }

    test("ERR-PBANK-23: continuing outputs with a non-constructor datum do not count as locked") {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-23")(
              eval(premium, ClaimPremiumOnCancel, refundTx(lockedDatum = d))
            )
    }

    test(
      "ClaimPremiumOnCancel: policy info datums that are not constructors are skipped, not a crash"
    ) {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-24.1")(
              eval(premium, ClaimPremiumOnCancel, refundTx(refs = Seq(policyInfoRef(d))))
            )
            val refs = Seq(policyInfoRef(d, 1), policyInfoRef(cancelled.toData, 2))
            assertSucceeds(eval(premium, ClaimPremiumOnCancel, refundTx(refs = refs)))
    }

    test("ERR-PBANK-24.1: no policy info reference input") {
        assertRejectedWith("ERR-PBANK-24.1")(
          eval(premium, ClaimPremiumOnCancel, refundTx(refs = Seq.empty))
        )
    }

    test("ERR-PBANK-24.2: two policy info reference inputs") {
        val refs = Seq(policyInfoRef(cancelled.toData, 1), policyInfoRef(cancelled.toData, 2))
        assertRejectedWith("ERR-PBANK-24.2")(
          eval(premium, ClaimPremiumOnCancel, refundTx(refs = refs))
        )
    }

    test("quirk: ERR-PBANK-32 (the fall-through code) also rejects a second refund") {
        val refunded = PBankPremium(share, refund)
        assertRejectedWith("ERR-PBANK-32")(
          eval(
            refunded,
            ClaimPremiumOnCancel,
            refundTx(taken = installment, input = refunded, inputLovelace = installment)
          )
        )
    }
}
