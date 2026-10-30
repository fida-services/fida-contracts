package fida.contract.insurance.lifecycle

import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyInitiated
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PolicyInitiatedRedeemer.PolicyInitiatedCancel
import fida.contract.toolbox.{Run, TestToolbox, User}
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.Validators.fidaCardsFromInts

/** Port of `Fida.Contract.Insurance.Lifecycle.FundingTest`. */
class FundingTest extends TestToolbox {

    private def cancelPolicy(iid: InsuranceId, signer: User)(using Run): Unit =
        runUpdatePolicyState(Cancelled, PolicyInitiated(PolicyInitiatedCancel), iid, signer)

    private def cancelPolicyIfIllegalState(state: InsurancePolicyState)(using Run): Unit =
        val users = setupUsers
        val iid = newSamplePolicy(users)
        runUpdatePolicyState(state, PolicyInitiated(PolicyInitiatedCancel), iid, users.broker1)

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

    good("Completing funding phase works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 10))
        triggerFundingComplete(iid, users)
    }
}
