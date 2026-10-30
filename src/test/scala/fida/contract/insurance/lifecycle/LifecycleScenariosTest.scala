package fida.contract.insurance.lifecycle

import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PolicyFundingRedeemer.PolicyFundingCancel
import fida.contract.insurance.PolicyInitiatedRedeemer.PolicyInitiatedCancel
import fida.contract.insurance.PolicyOnRiskRedeemer.PolicyOnRiskCancel
import fida.contract.toolbox.TestToolbox
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.Validators.fidaCardsFromInts

/** Policy lifecycle rules not covered by the original Haskell suite, checked end to end on the
  * emulator. The sample policy's authority is `AtLeastOneSign [fidaSystem, broker1]`.
  */
class LifecycleScenariosTest extends TestToolbox {

    // ---------------------------------------------------------------- Initiated

    good("Fida system can cancel an initiated policy") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        runUpdatePolicyState(
          Cancelled,
          PolicyInitiated(PolicyInitiatedCancel),
          iid,
          users.fidaSystem
        )
    }

    bad("Policy holder can't cancel an initiated policy", "ERR-INIT-1") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        runUpdatePolicyState(
          Cancelled,
          PolicyInitiated(PolicyInitiatedCancel),
          iid,
          users.policyHolder
        )
    }

    // Characterises the Haskell behaviour: AtLeastOneSign is satisfied by exactly one listed signer.
    bad(
      "AtLeastOneSign authority rejects a cancellation signed by two of its members",
      "ERR-INIT-1"
    ) {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        runUpdatePolicyState(
          Cancelled,
          PolicyInitiated(PolicyInitiatedCancel),
          iid,
          users.broker1,
          cosigners = Seq(users.fidaSystem)
        )
    }

    // ---------------------------------------------------------------- Funding

    good("Broker can cancel a funding policy") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        runUpdatePolicyState(Cancelled, PolicyFunding(PolicyFundingCancel), iid, users.broker1)
    }

    bad("Unauthorized user can't cancel a funding policy", "ERR-FUND-1") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        runUpdatePolicyState(Cancelled, PolicyFunding(PolicyFundingCancel), iid, users.investor1)
    }

    bad("Funding can't complete before every fida card is sold", "ERR-FUND-3") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 9))
        triggerFundingComplete(iid, users)
    }

    good("Insurance authority can complete funding") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 10))
        triggerFundingCompleteBy(iid, users.broker1)
    }

    bad(
      "Funding can't be completed by someone other than the policy holder or authority",
      "ERR-FUND-6"
    ) {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 10))
        triggerFundingCompleteBy(iid, users.investor1)
    }

    bad("Funding can't complete with a start date that is not in the past", "ERR-FUND-5") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 10))
        triggerFundingCompleteBy(iid, users.policyHolder, startDateShift = days(1))
    }

    // ---------------------------------------------------------------- Expiry

    bad("A funding policy can't expire before its funding deadline", "ERR-MAIN-2") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        triggerPolicyExpiration(iid, users.broker1)
    }

    bad("An on-risk policy can't expire before the end of its insurance period", "ERR-MAIN-2") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        waitUntil(currentTime + days(300))
        triggerPolicyExpiration(iid, users.broker1)
    }

    bad("A cancelled policy can't expire", "ERR-CANC-0") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        runUpdatePolicyState(Cancelled, PolicyInitiated(PolicyInitiatedCancel), iid, users.broker1)
        waitUntil(currentTime + days(8))
        triggerPolicyExpiration(iid, users.broker1)
    }

    // ---------------------------------------------------------------- OnRisk

    good("Insurance authority can cancel an on-risk policy") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        runUpdatePolicyState(Cancelled, PolicyOnRisk(PolicyOnRiskCancel), iid, users.fidaSystem)
    }

    bad("A claim can't be filed for a policy that is not on risk", "ERR-ON-RISK-13") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        fileClaim(iid, users.policyHolder)
    }

    bad("Only the policy holder can file a claim", "ERR-ON-RISK-1") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        fileClaim(iid, users.investor1)
    }

    bad("Only the insurance authority can accept a claim", "ERR-ON-RISK-2") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.policyHolder)
    }

    bad("A claim can't expire before its time to live", "ERR-ON-RISK-11") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        expireClaim(iid, users.investor1)
    }

    bad("Only the policy holder can collect claim payments", "ERR-ON-RISK-12") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        payForClaimWithCollateral(iid, users.investor1)
        claimPayment(iid, users.investor1)
    }
}
