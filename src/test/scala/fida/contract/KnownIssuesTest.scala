package fida.contract

import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.PolicyClaimPayment
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyInitiated
import fida.contract.insurance.InsurancePolicyState.Funding
import fida.contract.insurance.PiggyBankRedeemer.PayForClaimWithCollateral
import fida.contract.insurance.PolicyInitiatedRedeemer.PolicyInitiatedPayPremium
import fida.contract.toolbox.{Run, TestToolbox, TxFailure}
import fida.contract.toolbox.Actions.*
import fida.contract.toolbox.MakeInsurancePolicy.newSamplePolicy
import fida.contract.toolbox.Validators.*
import scalus.cardano.ledger.{AssetName, Value}

/** Vulnerabilities inherited from the Haskell contracts, which this migration ports faithfully.
  *
  * Each test states the secure behaviour (the attack transaction must be rejected) inside
  * `pendingUntilFixed`: while the contracts keep the Haskell behaviour the attack succeeds and the
  * test is reported as pending; once the contracts are fixed the test fails, as a reminder to drop
  * `pendingUntilFixed` and keep it as a regression test. See docs/MIGRATION.md.
  */
class KnownIssuesTest extends TestToolbox {

    private val card1 = fidaCardFromInt(1)
    private val card2 = fidaCardFromInt(2)

    /** `scenario` prepares the ledger and returns the attack; only the attack is expected to fail.
      */
    private def attack(name: String)(scenario: Run ?=> (() => Unit)): Unit =
        test(s"Known issue: $name") {
            given Run = new Run
            val attackTx = scenario
            pendingUntilFixed {
                intercept[TxFailure](attackTx())
            }
        }

    // Initiated.hs accepts PolicyInitiatedPayPremium for an InsuranceInfo datum in ANY state as long
    // as some input holds the POLICY-PAYMENT NFT. After the premium is paid that NFT sits in the
    // policy holder's wallet and can never be burned (the one-shot minting policy cannot run again),
    // so the holder can move an on-risk (or expired, or cancelled) policy back to Funding and then
    // restart the coverage without paying a new premium.
    attack("the premium payment NFT moves an on-risk policy back to Funding") {
        val run = summon[Run]
        import run.given
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        val paymentNft = run.utxosWithAsset(
          users.policyHolder,
          policyId(iid),
          AssetName(Tokens.policyPaymentTokenName)
        )
        val tx = paymentNft.foldLeft(
          updatePolicyStateTx(iid, iinfo(iid), Funding, PolicyInitiated(PolicyInitiatedPayPremium))
        )(_.spend(_))
        () => run.submitTx(users.policyHolder, tx)
    }

    // PiggyBank.hs SellFidaCard checks neither the policy state nor how much ADA leaves the piggy
    // bank: the card holder can sell the card back, and recover the whole collateral, while the
    // policy is on risk, leaving the policy holder uncovered.
    attack("a fida card can be sold back while the policy is on risk") {
        val users = setupUsers
        val iid = newSamplePolicy(users)
        startCoverage(iid, users)
        () => sellFidaCard(iid, users.investor1, card1)
    }

    // PiggyBank.hs PayForClaimWithCollateral sums every claim payment output of the transaction, so
    // several piggy banks spent together are all satisfied by the same payment (double
    // satisfaction): here two piggy banks release two shares of collateral for one share paid.
    attack("two piggy banks can be satisfied by a single claim payment") {
        val run = summon[Run]
        import run.given
        val users = setupUsers
        val iid = newSamplePolicy(users)
        createClaim(iid, users.policyHolder, users)
        acceptClaim(iid, users.fidaSystem)
        val info = insuranceInfo(iid)
        val claim = info.iInfoClaim.orFail("no claim")
        val share = Value.lovelace((claim.claimAmount / info.iInfoFidaCardNumber).toLong)
        val claimPaymentDatum: InsurancePolicyDatum = PolicyClaimPayment
        val tx = Seq(card1, card2).foldLeft(
          run.txBuilder
              .references(iinfo(iid).utxo)
              .payTo(insurancePolicyAddress(iid), share, claimPaymentDatum)
              .validFrom(run.instant(run.currentTime))
        ) { (b, fcid) =>
            val box = piggyBankInfo(iid, fcid)
            val paid =
                PiggyBankDatum.addPiggyBankPaidClaim(claim.claimId, box.datum).orFail("not a card")
            fidaCardUtxos(users.investor1, iid, fcid)
                .foldLeft(b)(_.spend(_))
                .payTo(users.investor1.address, fidaCardNFT(iid, fcid))
                .spend(box.utxo, PayForClaimWithCollateral, piggyBank(iid, fcid))
                .payTo(piggyBankAddr(iid, fcid), box.value - share, paid)
        }
        () => run.submitTx(users.investor1, tx)
    }
}
