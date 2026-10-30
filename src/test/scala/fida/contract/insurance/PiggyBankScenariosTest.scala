package fida.contract.insurance

import fida.contract.insurance.InsurancePolicyRedeemer.PolicyOnRisk
import fida.contract.insurance.InsurancePolicyState.Cancelled
import fida.contract.insurance.PiggyBankRedeemer.BuyFidaCard
import fida.contract.insurance.PolicyOnRiskRedeemer.PolicyOnRiskCancel
import fida.contract.toolbox.{Run, TestToolbox}
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.Validators.*
import scalus.cardano.ledger.Value

/** Piggy bank rules not covered by the original Haskell suite, checked end to end on the
  * emulator. Each piggy bank of the sample policy holds a 1 000 ADA fida card and receives a 20 ADA
  * premium, unlocked by 5 ADA every 90 days (fully after 270 days).
  */
class PiggyBankScenariosTest extends TestToolbox {

    private val card1 = fidaCardFromInt(1)
    private val card2 = fidaCardFromInt(2)

    bad("A sold fida card can't be bought again", "ERR-PBANK-32") {
        val run = summon[Run]
        import run.given
        val users = setupUsers
        val iid = newSamplePolicy(users)
        buyFidaCard(iid, users.investor1, card1)
        val box = piggyBankInfo(iid, card1)
        val sold = PiggyBankDatum.setFidaCardSold(box.datum).orFail("Can't buy fida card")
        run.submitTx(
          users.investor2,
          run.txBuilder
              .spend(box.utxo, BuyFidaCard, piggyBank(iid, card1))
              .payTo(piggyBankAddr(iid, card1), box.value + Value.ada(1_000), sold)
        )
    }

    bad("Premium can't be claimed before the first installment is due", "ERR-PBANK-8") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        claimPremium(iid, card1, users.investor1, ada(5))
    }

    bad("More premium than unlocked can't be claimed", "ERR-PBANK-8") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        waitUntil(currentTime + days(90))
        claimPremium(iid, card1, users.investor1, ada(10))
    }

    bad("Premium can only be claimed by the fida card owner", "ERR-PBANK-7") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        waitUntil(currentTime + days(90))
        claimPremiumWith(iid, card1, users.investor2, ada(5), withCard = false)
    }

    good("The whole premium is unlocked once every installment is due") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        waitUntil(currentTime + days(271))
        claimPremium(iid, card1, users.investor1, ada(20))
    }

    bad("Only the policy holder can take the refund of a cancelled policy", "ERR-PBANK-22") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        waitUntil(currentTime + days(90))
        runUpdatePolicyState(Cancelled, PolicyOnRisk(PolicyOnRiskCancel), iid, users.broker1)
        claimPremiumOnCancel(iid, card1, users.investor1, ada(15))
    }

    bad("The policy holder can't take more than the refund of a cancelled policy", "ERR-PBANK-23") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        waitUntil(currentTime + days(90))
        runUpdatePolicyState(Cancelled, PolicyOnRisk(PolicyOnRiskCancel), iid, users.broker1)
        claimPremiumOnCancel(iid, card1, users.policyHolder, ada(16))
    }

    bad("Collateral can't pay a claim that has not been accepted", "ERR-PBANK-11") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        payForClaimWithCollateral(iid, users.investor1)
    }

    bad("A piggy bank can't pay more than its share of a claim", "ERR-PBANK-12") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        payForClaimWithCollateralOf(iid, users.investor1, Seq(card1), share = Some(ada(2)))
    }

    bad("A piggy bank can't pay the same claim twice", "ERR-PBANK-13") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        payForClaimWithCollateralOf(iid, users.investor1, Seq(card1))
        payForClaimWithCollateralOf(iid, users.investor1, Seq(card1))
    }

    bad("Collateral can't be unlocked while the policy is on risk", "ERR-PBANK-28") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        unlockCollateralOnExpired(users.investor1, iid, card1)
    }

    bad("Collateral can't be unlocked through cancellation of a policy that is not cancelled", "ERR-PBANK-25") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        unlockCollateralOnCancel(users.investor1, iid, card1)
    }

    good("Collateral is unlocked after expiry once the claim has been paid") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        payForClaimWithCollateral(iid, users.investor1)
        waitUntil(currentTime + days(366))
        triggerPolicyExpiration(iid, users.broker1)
        unlockCollateralsOnExpired(users.investor1, iid, fidaCardsFromInts(1 to 10))
    }

    bad("Collateral stays locked after expiry while the policy's claim is unpaid", "ERR-PBANK-30") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        waitUntil(currentTime + days(366))
        triggerPolicyExpiration(iid, users.broker1)
        unlockCollateralOnExpired(users.investor1, iid, card2)
    }
}
