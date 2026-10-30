package fida.contract.insurance.piggybank

import fida.contract.insurance.{ClaimInfo, PiggyBankDatum}
import fida.contract.insurance.InsurancePolicyDatum.PolicyClaimPayment
import fida.contract.insurance.InsurancePolicyState.OnRisk
import fida.contract.insurance.PiggyBankDatum.{PBankFidaCard, PBankPremium}
import fida.contract.insurance.PiggyBankRedeemer.PayForClaimWithCollateral
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Address, Interval}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.{List, Option}

/** `PayForClaimWithCollateral` on a sold `PBankFidaCard`: ERR-PBANK-11 to 20. */
class PiggyBankPayForClaimWithCollateralSpec extends PiggyBankSpecBase {

    private val acceptedClaim: ClaimInfo = sampleClaim.copy(claimAccepted = true)
    private val claimId = acceptedClaim.claimId

    private def policyWith(claim: Option[ClaimInfo]) =
        sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(beginningOfTime), claim = claim)

    private val onRisk = policyWith(Option.Some(acceptedClaim))

    /** Each piggy bank pays its share of the claim: 10 ADA / 10 cards. */
    private val share: BigInt = acceptedClaim.claimAmount / fidaCardQuantity
    private val collateral: BigInt = minAda + fidaCardValue

    private val sold = cardDatum(isSold = true)
    private val paid = cardDatum(isSold = true, paidClaims = Seq(claimId))
    private val claimPayment: Data = PolicyClaimPayment.toData

    /** After the claim time to pay (claim date + 50 s), anyone may pay the claim. */
    private val afterTimeToPay = Interval.after(acceptedClaim.claimDate + seconds(50) + 1)

    private def statusOutput(datum: Data = paid.toData, lovelace: BigInt = collateral - share): TxOut =
        piggyBankOutput(datum, value(lovelace, statusNft))

    private def payment(amount: BigInt = share, address: Address = policyAddress, datum: Data = claimPayment): TxOut =
        txOut(address, value(amount), datum)

    private def pay(
        outputs: Seq[TxOut] = Seq(statusOutput(), payment()),
        input: PiggyBankDatum = sold,
        refs: Seq[TxInInfo] = Seq(policyInfoRef(onRisk.toData)),
        owner: Seq[TxInInfo] = Seq(cardOwnerInput),
        validRange: Interval = Interval.always
    ): ScriptContext =
        spendPiggyBank(
          Seq(piggyBankInput(input.toData, value(collateral, statusNft))) ++ owner,
          refs = refs,
          outputs = outputs,
          validRange = validRange
        )

    test("PayForClaimWithCollateral: the card owner pays the claim share out of the collateral") {
        assertSucceeds(eval(sold, PayForClaimWithCollateral, pay()))
    }

    test("PayForClaimWithCollateral: after the claim time to pay anyone may pay") {
        assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(owner = Seq.empty, validRange = afterTimeToPay)))
    }

    test("quirk: one claim payment output satisfies every piggy bank spent in the same transaction") {
        val card2 = fidaCardId(2)
        val address2 = scriptAddress(scriptHash(102))
        val ref2 = outRef(102)
        val sold2 = PBankFidaCard(true, card2, fidaCardValue, List.Nil)
        val paid2 = PBankFidaCard(true, card2, fidaCardValue, List(claimId))
        val info = txInfo(
          inputs = Seq(
            piggyBankInput(sold.toData, value(collateral, statusNft)),
            txIn(ref2, txOut(address2, value(collateral, statusNft), sold2.toData))
          ),
          referenceInputs = Seq(policyInfoRef(onRisk.toData)),
          // both piggy banks release one share, but a single share is paid
          outputs = Seq(statusOutput(), txOut(address2, value(collateral - share, statusNft), paid2.toData), payment()),
          validRange = afterTimeToPay
        )
        assertSucceeds(eval(sold, PayForClaimWithCollateral, spending(info, piggyBankRef)))
        assertSucceeds(evalRaw(sold2.toData, PayForClaimWithCollateral.toData, spending(info, ref2), card = card2.toData))
    }

    test("ERR-PBANK-11: the claim is not accepted") {
        val notAccepted = policyWith(Option.Some(sampleClaim))
        assertRejectedWith("ERR-PBANK-11")(
          eval(sold, PayForClaimWithCollateral, pay(refs = Seq(policyInfoRef(notAccepted.toData))))
        )
    }

    test("ERR-PBANK-12: more than the claim share leaves the collateral") {
        assertRejectedWith("ERR-PBANK-12")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(lovelace = collateral - share - 1), payment())))
        )
    }

    test("ERR-PBANK-13: the claim was already paid by this piggy bank") {
        val twice = cardDatum(isSold = true, paidClaims = Seq(claimId, claimId))
        assertRejectedWith("ERR-PBANK-13")(
          eval(paid, PayForClaimWithCollateral, pay(input = paid, outputs = Seq(statusOutput(twice.toData), payment())))
        )
    }

    test("ERR-PBANK-14: the output datum does not record the claim as paid") {
        assertRejectedWith("ERR-PBANK-14")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(sold.toData), payment())))
        )
    }

    test("ERR-PBANK-15: the claim payment is too small") {
        assertRejectedWith("ERR-PBANK-15")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(), payment(amount = share - 1))))
        )
    }

    test("ERR-PBANK-15: payments elsewhere or without the exact PolicyClaimPayment datum do not count") {
        val outputs = Seq(
          statusOutput(),
          payment(address = investorAddress),
          payment(datum = onRisk.toData),
          payment(datum = withExtraField(claimPayment)),
          txOutNoDatum(policyAddress, value(share))
        )
        assertRejectedWith("ERR-PBANK-15")(eval(sold, PayForClaimWithCollateral, pay(outputs = outputs)))
    }

    test("ERR-PBANK-16: before the claim time to pay only the card owner may pay") {
        val beforeTimeToPay = Interval.after(acceptedClaim.claimDate + seconds(50))
        assertRejectedWith("ERR-PBANK-16")(eval(sold, PayForClaimWithCollateral, pay(owner = Seq.empty)))
        assertRejectedWith("ERR-PBANK-16")(
          eval(sold, PayForClaimWithCollateral, pay(owner = Seq.empty, validRange = beforeTimeToPay))
        )
    }

    test("ERR-PBANK-17: the output datum changes the card value") {
        val cheaper = cardDatum(isSold = true, cardValue = fidaCardValue - 1, paidClaims = Seq(claimId))
        assertRejectedWith("ERR-PBANK-17")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(cheaper.toData), payment())))
        )
    }

    test("ERR-PBANK-18: no policy info reference input") {
        assertRejectedWith("ERR-PBANK-18")(eval(sold, PayForClaimWithCollateral, pay(refs = Seq.empty)))
    }

    test("ERR-PBANK-18: the policy has no claim") {
        val noClaim = policyWith(Option.None)
        assertRejectedWith("ERR-PBANK-18")(
          eval(sold, PayForClaimWithCollateral, pay(refs = Seq(policyInfoRef(noClaim.toData))))
        )
    }

    test("ERR-PBANK-18: two policy info reference inputs") {
        val refs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(onRisk.toData, 2))
        assertRejectedWith("ERR-PBANK-18")(eval(sold, PayForClaimWithCollateral, pay(refs = refs)))
    }

    test("ERR-PBANK-18: any second policy info datum that decodes counts, e.g. a PremiumPaymentInfo") {
        val refs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(samplePremiumPaymentInfo().toData, 2))
        assertRejectedWith("ERR-PBANK-18")(eval(sold, PayForClaimWithCollateral, pay(refs = refs)))
        val claimPaymentRefs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(claimPayment, 2))
        assertRejectedWith("ERR-PBANK-18")(eval(sold, PayForClaimWithCollateral, pay(refs = claimPaymentRefs)))
    }

    test("PayForClaimWithCollateral: policy info datums that do not decode exactly are ignored") {
        val premiumInfo = samplePremiumPaymentInfo().toData
        // an address whose credential has an unknown constructor
        val badAddress = constrData(0, constrData(2, Data.B(cs)), constrData(1))
        val undecodable = Seq(
          withExtraField(premiumInfo),
          withField(premiumInfo, 1, listData(badAddress)),
          withExtraField(claimPayment),
          withExtraField(onRisk.toData)
        )
        for d <- undecodable do
            assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(refs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(d, 2)))))
    }

    test("PayForClaimWithCollateral: policy info datums that are not constructors are skipped, not a crash") {
        for d <- nonConstructorData do
            assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(refs = Seq(policyInfoRef(d, 1), policyInfoRef(onRisk.toData, 2)))))
            assertRejectedWith("ERR-PBANK-18")(eval(sold, PayForClaimWithCollateral, pay(refs = Seq(policyInfoRef(d)))))
    }

    test("PayForClaimWithCollateral: a PremiumPaymentInfo counts only when every address decodes") {
        val premiumInfo = samplePremiumPaymentInfo().toData
        val pkh = Data.B(investor1.hash)
        val stakingHash = constrData(0, constrData(0, constrData(0, pkh))) // Just (StakingHash (PubKeyCredential _))
        val stakingPtr = constrData(0, constrData(1, Data.I(1), Data.I(2), Data.I(3))) // Just (StakingPtr 1 2 3)
        def withAddresses(addresses: Data*) = withField(premiumInfo, 1, listData(addresses*))
        def address(credential: Data, staking: Data) = constrData(0, credential, staking)
        val credential = constrData(0, pkh)
        // well formed, so it is a second policy info datum
        for staking <- Seq(constrData(1), stakingHash, stakingPtr) do
            val refs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(withAddresses(address(credential, staking)), 2))
            assertRejectedWith("ERR-PBANK-18")(eval(sold, PayForClaimWithCollateral, pay(refs = refs)))
        // not constructors where constructors are expected, so skipped
        val i = Data.I(0)
        val skipped = Seq(
          withField(premiumInfo, 1, i), // ppInfoPiggyBanks
          withAddresses(i), // an address
          withAddresses(address(i, constrData(1))), // a credential
          withAddresses(address(constrData(0, i), constrData(1))), // a hash
          withAddresses(address(credential, i)), // the optional staking credential
          withAddresses(address(credential, constrData(0, i))), // a staking credential
          withAddresses(address(credential, constrData(0, constrData(0, i)))), // StakingHash of a non-credential
          withAddresses(address(credential, constrData(0, constrData(1, Data.I(1), Data.I(2), pkh)))) // StakingPtr
        )
        for d <- skipped do
            assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(refs = Seq(policyInfoRef(onRisk.toData, 1), policyInfoRef(d, 2)))))
    }

    test("PayForClaimWithCollateral: status outputs with a non-constructor datum are skipped (ERR-PBANK-20)") {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-20")(
              eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(d), payment())))
            )
            assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(d), statusOutput(), payment()))))
    }

    test("PayForClaimWithCollateral: claim payments with a non-constructor datum do not count (ERR-PBANK-15)") {
        val notPayments = nonConstructorData.map(d => payment(datum = d))
        assertRejectedWith("ERR-PBANK-15")(eval(sold, PayForClaimWithCollateral, pay(outputs = statusOutput() +: notPayments)))
        assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(outputs = statusOutput() +: payment() +: notPayments)))
    }

    test("PayForClaimWithCollateral: unrelated outputs with non-constructor datums are ignored") {
        val unrelated = nonConstructorData.map(d => txOut(investorAddress, value(minAda), d))
        assertSucceeds(eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(), payment()) ++ unrelated)))
    }

    test("ERR-PBANK-19: no own input (non-spending context), reported after the claim checks") {
        val info = txInfo(
          inputs = Seq(piggyBankInput(sold.toData, value(collateral, statusNft)), cardOwnerInput),
          referenceInputs = Seq(policyInfoRef(onRisk.toData)),
          outputs = Seq(statusOutput(), payment())
        )
        assertRejectedWith("ERR-PBANK-19")(eval(sold, PayForClaimWithCollateral, minting(info, cs)))
    }

    test("ERR-PBANK-20: no continuing output holds the status NFT") {
        val noStatus = piggyBankOutput(paid.toData, value(collateral - share))
        assertRejectedWith("ERR-PBANK-20")(eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(noStatus, payment()))))
    }

    test("ERR-PBANK-20: the status output datum is not a PBankFidaCard") {
        val premium = PBankPremium(1, 0)
        assertRejectedWith("ERR-PBANK-20")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(premium.toData), payment())))
        )
    }

    test("ERR-PBANK-20: a status output datum with an extra field does not decode (exact fromBuiltinData)") {
        assertRejectedWith("ERR-PBANK-20")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(withExtraField(paid.toData)), payment())))
        )
    }

    test("ERR-PBANK-20: two continuing outputs hold a status NFT") {
        assertRejectedWith("ERR-PBANK-20")(
          eval(sold, PayForClaimWithCollateral, pay(outputs = Seq(statusOutput(), statusOutput(), payment())))
        )
    }
}
