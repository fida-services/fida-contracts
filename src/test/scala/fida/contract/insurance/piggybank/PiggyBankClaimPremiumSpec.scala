package fida.contract.insurance.piggybank

import fida.contract.insurance.InsurancePolicyState.{Funding, OnRisk}
import fida.contract.insurance.PiggyBankDatum.PBankPremium
import fida.contract.insurance.PiggyBankRedeemer.ClaimPremium
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Interval, IntervalBound, PosixTime}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo}
import scalus.cardano.onchain.plutus.prelude.Option

/** `ClaimPremium` on a `PBankPremium`: ERR-PBANK-7 to 10. */
class PiggyBankClaimPremiumSpec extends PiggyBankSpecBase {

    /** Premium paid into each piggy bank (`ppInfoPremiumAmountPerPiggyBank`): 20 ADA. */
    private val share: BigInt = policyPrice / fidaCardQuantity

    /** Premium unlocked per installment (`InstallmentsInfo` amount): 5 ADA. */
    private val installment: BigInt = share / 4

    private val premium = PBankPremium(share, 0)
    private val start: PosixTime = beginningOfTime
    private val onRisk = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(start))
    private val afterFirstInstallment = Interval.after(start + days(90) + 1)

    /** The card owner claims `inputLovelace - locked`, leaving `locked` in a continuing output. */
    private def claim(
        locked: BigInt,
        input: Data = premium.toData,
        inputLovelace: BigInt = share,
        lockedDatum: Data = premium.toData,
        refs: Seq[TxInInfo] = Seq(policyInfoRef(onRisk.toData)),
        owner: Seq[TxInInfo] = Seq(cardOwnerInput),
        validRange: Interval = afterFirstInstallment
    ): ScriptContext =
        val lockedOutput = if locked > 0 then Seq(piggyBankOutput(lockedDatum, value(locked))) else Seq.empty
        spendPiggyBank(
          Seq(piggyBankInput(input, value(inputLovelace))) ++ owner,
          refs = refs,
          outputs = lockedOutput :+ txOutNoDatum(investorAddress, value(inputLovelace - locked, cardNft)),
          validRange = validRange
        )

    test("ClaimPremium: the card owner claims the installments that are due") {
        assertSucceeds(eval(premium, ClaimPremium, claim(locked = share - installment)))
    }

    test("ClaimPremium: the whole premium unlocks once every installment is due") {
        val afterLastInstallment = Interval.after(start + days(270) + 1)
        assertSucceeds(eval(premium, ClaimPremium, claim(locked = 0, validRange = afterLastInstallment)))
    }

    test("ClaimPremium: an installment is due only strictly after its date (Interval.before)") {
        val dueDate = start + days(90)
        val fromDueDate = Interval.after(dueDate)
        val afterDueDate = Interval(IntervalBound.finiteExclusive(dueDate), IntervalBound.posInf)
        assertRejectedWith("ERR-PBANK-8")(
          eval(premium, ClaimPremium, claim(locked = share - installment, validRange = fromDueDate))
        )
        assertSucceeds(eval(premium, ClaimPremium, claim(locked = share - installment, validRange = afterDueDate)))
    }

    test("ClaimPremium: a refund lowers the premium that must stay locked") {
        val refunded = PBankPremium(share, share - installment)
        assertSucceeds(
          eval(refunded, ClaimPremium, claim(locked = 0, input = refunded.toData, inputLovelace = installment))
        )
    }

    test("ERR-PBANK-7: the fida card NFT is not spent") {
        assertRejectedWith("ERR-PBANK-7")(eval(premium, ClaimPremium, claim(locked = share, owner = Seq.empty)))
    }

    test("ERR-PBANK-7: the fida card NFT is spent twice") {
        val secondCard = txIn(outRef(202), txOutNoDatum(investorAddress, value(minAda, cardNft)))
        assertRejectedWith("ERR-PBANK-7")(
          eval(premium, ClaimPremium, claim(locked = share, owner = Seq(cardOwnerInput, secondCard)))
        )
    }

    test("ERR-PBANK-8: claiming more than the unlocked premium") {
        assertRejectedWith("ERR-PBANK-8")(eval(premium, ClaimPremium, claim(locked = share - installment - 1)))
    }

    test("ERR-PBANK-8: a continuing output with another datum does not count as locked") {
        val other = PBankPremium(share, 1)
        assertRejectedWith("ERR-PBANK-8")(
          eval(premium, ClaimPremium, claim(locked = share - installment, lockedDatum = other.toData))
        )
    }

    test("ERR-PBANK-8: locked premium counts the canonical re-encoding of a datum with an extra field") {
        val extra = withExtraField(premium.toData)
        assertSucceeds(evalRaw(extra, ClaimPremium.toData, claim(locked = share - installment, input = extra)))
        assertRejectedWith("ERR-PBANK-8")(
          evalRaw(extra, ClaimPremium.toData, claim(locked = share - installment, input = extra, lockedDatum = extra))
        )
    }

    test("ERR-PBANK-8: continuing outputs with a non-constructor datum do not count as locked") {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-8")(
              eval(premium, ClaimPremium, claim(locked = share - installment, lockedDatum = d))
            )
    }

    test("ClaimPremium: policy info datums that are not constructors are skipped, not a crash") {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-9.1")(
              eval(premium, ClaimPremium, claim(locked = share, refs = Seq(policyInfoRef(d))))
            )
            assertSucceeds(
              eval(
                premium,
                ClaimPremium,
                claim(locked = share - installment, refs = Seq(policyInfoRef(d, 1), policyInfoRef(onRisk.toData, 2)))
              )
            )
    }

    test("ERR-PBANK-9.1: no policy info reference input") {
        assertRejectedWith("ERR-PBANK-9.1")(eval(premium, ClaimPremium, claim(locked = share, refs = Seq.empty)))
    }

    test("ERR-PBANK-9.1: a policy info datum that does not decode exactly is ignored") {
        val refs = Seq(policyInfoRef(withExtraField(onRisk.toData)))
        assertRejectedWith("ERR-PBANK-9.1")(eval(premium, ClaimPremium, claim(locked = share, refs = refs)))
    }

    test("ERR-PBANK-9.2: two policy info reference inputs") {
        val refs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(onRisk.toData, 2))
        assertRejectedWith("ERR-PBANK-9.2")(eval(premium, ClaimPremium, claim(locked = share, refs = refs)))
    }

    test("ERR-PBANK-10: the policy has not started (no start date)") {
        val refs = Seq(policyInfoRef(sampleInsuranceInfo(state = Funding).toData))
        assertRejectedWith("ERR-PBANK-10")(eval(premium, ClaimPremium, claim(locked = share, refs = refs)))
    }
}
