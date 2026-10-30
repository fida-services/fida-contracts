package fida.contract

import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.TestToolbox
import fida.contract.toolbox.Validators.fidaCardsFromInts

/** Port of `Fida.Contract.FidaPolicyContractTest`. */
class FidaPolicyContractTest extends TestToolbox {

    good("Expiration policy during Funding phase works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        val time = currentTime
        waitUntil(time + days(7))
        triggerPolicyExpiration(iid, users.broker1)
    }

    good("Expiration policy during OnRisk phase works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 10))
        triggerFundingComplete(iid, users)
        val time = currentTime
        waitUntil(time + days(366))
        triggerPolicyExpiration(iid, users.broker1)
    }
}
