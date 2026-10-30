package fida.contract.insurance.lifecycle

import fida.contract.toolbox.TestToolbox
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy

/** Port of `Fida.Contract.Insurance.Lifecycle.OnRiskTest`. */
class OnRiskTest extends TestToolbox {

    good("Policy holder can create claim") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
    }

    good("Policy holder can close claim") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        closeClaim(iid, users.policyHolder)
    }

    good("Fida System can accept claim") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
    }

    good("Investor can expire claim") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        waitNSlots(100)
        expireClaim(iid, users.investor1)
    }

    good("Fida System can fail claim") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        waitNSlots(100)
        failClaim(iid, users.fidaSystem)
    }

    good("Policy holder can claim payment") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        payForClaimWithCollateral(iid, users.investor1)
        claimPayment(iid, users.policyHolder)
    }
}
