package fida.contract.insurance

import fida.contract.insurance.InsurancePolicyRedeemer.PolicyOnRisk
import fida.contract.insurance.InsurancePolicyState.Cancelled
import fida.contract.insurance.PolicyOnRiskRedeemer.PolicyOnRiskCancel
import fida.contract.toolbox.TestToolbox
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.Validators.{fidaCardFromInt, fidaCardsFromInts}

/** Port of `Fida.Contract.Insurance.PiggyBankTest`. */
class PiggyBankTest extends TestToolbox {

    good("Buying Fida card works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 5))
    }

    good("Sell Fida card works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 3))
        sellFidaCard(iid, users.investor1, fidaCardFromInt(1))
    }

    good("Claim premium works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        val fidaCards = fidaCardsFromInts(1 to 10)
        buyFidaCards(iid, users.investor1, fidaCards)
        triggerFundingComplete(iid, users)
        val time = currentTime
        waitUntil(time + days(90))
        claimPremium(iid, fidaCards.head, users.investor1, ada(5))
    }

    good("Claim premium on cancel works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        val fidaCards = fidaCardsFromInts(1 to 10)
        buyFidaCards(iid, users.investor1, fidaCards)
        triggerFundingComplete(iid, users)
        val time = currentTime
        waitUntil(time + days(90))
        runUpdatePolicyState(Cancelled, PolicyOnRisk(PolicyOnRiskCancel), iid, users.broker1)
        claimPremium(iid, fidaCards(0), users.investor1, ada(5))
        claimPremiumOnCancel(iid, fidaCards(0), users.policyHolder, ada(15))
        claimPremiumOnCancel(iid, fidaCards(1), users.policyHolder, ada(15))
        claimPremium(iid, fidaCards(1), users.investor1, ada(5))
    }

    good("Pay for claim with collaterl works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        payForClaimWithCollateral(iid, users.investor1)
    }

    good("Unlock collateral on cancel works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        buyFidaCards(iid, users.investor1, Seq(fidaCardFromInt(1)))
        buyFidaCards(iid, users.investor2, Seq(fidaCardFromInt(2)))
        runUpdatePolicyState(Cancelled, PolicyOnRisk(PolicyOnRiskCancel), iid, users.broker1)
        unlockCollateralOnCancel(users.investor1, iid, fidaCardFromInt(1))
        unlockCollateralOnCancel(users.investor2, iid, fidaCardFromInt(2))
    }

    good("Unlock collateral works") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        payPremium(iid, users.policyHolder)
        val fidaCards = fidaCardsFromInts(1 to 10)
        buyFidaCards(iid, users.investor1, fidaCards)
        triggerFundingComplete(iid, users)
        val time = currentTime
        waitUntil(time + days(366))
        triggerPolicyExpiration(iid, users.broker1)
        unlockCollateralsOnExpired(users.investor1, iid, fidaCards)
    }
}
