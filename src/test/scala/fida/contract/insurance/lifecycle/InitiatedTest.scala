package fida.contract.insurance.lifecycle

import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PremiumPaymentInfo}
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyInitiated
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PiggyBankDatum.PBankFidaCard
import fida.contract.insurance.PolicyInitiatedRedeemer.PolicyInitiatedCancel
import fida.contract.toolbox.{Run, TestToolbox, User}
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.Validators.*
import scalus.cardano.onchain.plutus.prelude.Option as POption

/** Port of `Fida.Contract.Insurance.Lifecycle.InitiatedTest`. */
class InitiatedTest extends TestToolbox {

    private def cancelPolicy(iid: InsuranceId, signer: User)(using Run): Unit =
        runUpdatePolicyState(Cancelled, PolicyInitiated(PolicyInitiatedCancel), iid, signer)

    private def cancelPolicyIfIllegalState(state: InsurancePolicyState)(using Run): Unit =
        val users = setupUsers
        val iid = newSamplePolicy(users)
        runUpdatePolicyState(state, PolicyInitiated(PolicyInitiatedCancel), iid, users.broker1)

    good("Creating policy works") {
        newSamplePolicy(setupUsers)
    }

    good("Broker is allowed to cancell a policy") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        cancelPolicy(iid, users.broker1)
    }

    bad("Unauthorized user is not allowed to cancell a policy", "ERR-INIT-1") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        cancelPolicy(iid, users.investor1)
    }

    bad("Cancelling policy can't set state to Funding", "ERR-INIT-2") {
        cancelPolicyIfIllegalState(Funding)
    }

    bad("Cancelling policy can't set state to OnRisk", "ERR-INIT-2") {
        cancelPolicyIfIllegalState(OnRisk)
    }

    bad("Cancelling policy can't set state to Initiated", "ERR-INIT-2") {
        cancelPolicyIfIllegalState(Initiated)
    }

    good("Paying premium works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
    }

    good("All required utxos are there") {
        val run = summon[Run]
        import run.given
        val users = setupUsers
        val iid = newSamplePolicy(users)
        val iiBox = iinfo(iid)
        val piBox = ppInfo(iid)
        (iiBox.datum, piBox.datum) match
            case (info: InsuranceInfo, payment: PremiumPaymentInfo) =>
                assert(quantity(iiBox.value, iid, Tokens.policyInfoTokenName) == 1, "No insurance info token")
                assert(quantity(piBox.value, iid, Tokens.policyPaymentTokenName) == 1, "No insurance payment info token")
                assert(info.iInfoFidaCardValue == BigInt(1_000_000_000), "Fida card value doesn't match")
                assert(info.iInfoCollateralAmount == BigInt(10_000_000_000L), "Collateral amount doesn't match")
                assert(info.iInfoFidaCardNumber == BigInt(10), "Fida card quantity doesn't match")
                assert(info.iInfoPremiumAmount == BigInt(200_000_000), "Premium amount doesn't match")
                assert(info.iInfoStartDate == POption.None, "Start date must be not set")
                assert(info.iInfoClaim == POption.None, "Claim must be not set")
                assert(info.iInfoState == Initiated, "State must be set to Initiated")
                assert(
                  payment.ppInfoPremiumAmountPerPiggyBank == BigInt(20_000_000),
                  "Premium amount per piggy bank doesn't match"
                )
                assert(
                  payment.ppInfoPiggyBanks.toScalaList ==
                      fidaCardsFromInts(1 to 10).map(c => plutusAddress(piggyBankAddr(iid, c))).toList,
                  "Piggy banks addresses don't match"
                )
                for fcid <- fidaCardsFromInts(1 to 10) do
                    val pbBox = piggyBankInfo(iid, fcid)
                    pbBox.datum match
                        case card: PBankFidaCard =>
                            assert(
                              quantity(pbBox.value, iid, Tokens.fidaCardTokenName(fcid.id)) == 1,
                              s"No piggy bank token @ piggy bank $fcid"
                            )
                            assert(card.pbfcFidaCardId == fcid, s"Fida card id doesn't match @ piggy bank $fcid")
                            assert(
                              card.pbfcFidaCardValue == BigInt(1_000_000_000),
                              s"Fida card value doesn't match @ piggy bank $fcid"
                            )
                            assert(!card.pbfcIsSold, s"Fida card must be available for selling @ piggy bank $fcid")
                            assert(card.pbfcPaidClaims.isEmpty, s"There should be no paid claims @ piggy bank $fcid")
                        case _ => fail("Datum for piggy bank dosen't match")
            case _ => fail("Datum types for insurance policy script don't match")
    }
}
