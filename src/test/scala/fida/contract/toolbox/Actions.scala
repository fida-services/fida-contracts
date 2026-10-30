package fida.contract.toolbox

import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.*
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.PiggyBankDatum.*
import fida.contract.insurance.PiggyBankRedeemer.*
import fida.contract.insurance.PolicyFundingRedeemer.PolicyFundingFundingComplete
import fida.contract.insurance.PolicyInitiatedRedeemer.PolicyInitiatedPayPremium
import fida.contract.insurance.PolicyOnRiskRedeemer.*
import fida.contract.toolbox.Validators.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash, Utxo, Value}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.v1
import scalus.cardano.onchain.plutus.v1.PosixTime
import scalus.cardano.onchain.plutus.prelude.Option as POption

/** Transactions driving the contracts (port of `Fida.Contract.TestToolbox.Action` and the claim
  * helpers of `Fida.Contract.Insurance.Lifecycle.OnRiskTest`). Each action builds the same
  * transaction as its Haskell counterpart and submits it signed by the given user.
  */
object Actions {

    extension [A](option: POption[A])
        def orFail(message: String): A = option match
            case POption.Some(a) => a
            case POption.None    => throw new IllegalStateException(message)

    // ------------------------------------------------------------------ lookups

    def iinfo(iid: InsuranceId)(using run: Run): Box[InsurancePolicyDatum] =
        import run.given
        run.box[InsurancePolicyDatum](insurancePolicyAddress(iid), "policy info")(iinfoBox(iid))

    def insuranceInfo(iid: InsuranceId)(using Run): InsuranceInfo =
        iinfo(iid).datum match
            case info: InsuranceInfo => info
            case other               => throw new IllegalStateException(s"Unexpected policy datum $other")

    def ppInfo(iid: InsuranceId)(using run: Run): Box[InsurancePolicyDatum] =
        import run.given
        run.box[InsurancePolicyDatum](insurancePolicyAddress(iid), "premium payment info")(ppInfoBox(iid))

    def piggyBankInfo(iid: InsuranceId, fcid: FidaCardId)(using run: Run): Box[PiggyBankDatum] =
        import run.given
        run.box[PiggyBankDatum](piggyBankAddr(iid, fcid), "piggy bank info")(piggyBankInfoBox(iid, fcid))

    def premiumBox(iid: InsuranceId, fcid: FidaCardId)(using run: Run): Box[PiggyBankDatum] =
        import run.given
        run.box[PiggyBankDatum](piggyBankAddr(iid, fcid), "premium")(pbPremiumBox)

    /** The UTxO holding the insurance policy reference script (`withRefScript`). */
    def insuranceRefScript(iid: InsuranceId)(using run: Run): Utxo =
        import run.given
        val script = insurancePolicy(iid)
        run.utxosAt(insurancePolicyAddress(iid))
            .find(_.output.scriptRef.exists(_.script == script))
            .getOrElse(throw new NoSuchElementException("No reference script found"))

    /** The user's UTxOs holding the fida card NFT (`spend user (fidaCardNFT iid fcid)`). */
    def fidaCardUtxos(user: User, iid: InsuranceId, fcid: FidaCardId)(using run: Run): Seq[Utxo] =
        val utxos = run.utxosWithAsset(user, policyId(iid), AssetName(Tokens.fidaCardTokenName(fcid.id)))
        if utxos.isEmpty then throw new NoSuchElementException(s"$user does not hold fida card $fcid")
        utxos

    /** The ledger form of an on-chain address (no staking part). */
    def ledgerAddress(address: v1.Address)(using run: Run): Address =
        import run.given
        val credential = address.credential match
            case v1.Credential.PubKeyCredential(pkh) =>
                Credential.KeyHash(scalus.cardano.ledger.AddrKeyHash.fromByteString(pkh.hash))
            case v1.Credential.ScriptCredential(hash) => Credential.ScriptHash(ScriptHash.fromByteString(hash))
        Address(run.env.network, credential)

    private def now(using run: Run) = run.instant(run.currentTime)

    private def spendAll(builder: TxBuilder, utxos: Seq[Utxo]): TxBuilder =
        utxos.foldLeft(builder)(_.spend(_))

    // ------------------------------------------------------------------ policy state

    def updatePolicyStateTx(
        iid: InsuranceId,
        box: Box[InsurancePolicyDatum],
        state: InsurancePolicyState,
        r: InsurancePolicyRedeemer
    )(using run: Run): TxBuilder =
        import run.given
        val datum = InsurancePolicyDatum.updatePolicyState(box.datum, state).orFail("Can't update policy state")
        run.txBuilder
            .spend(box.utxo, r, insurancePolicy(iid))
            .payTo(insurancePolicyAddress(iid), box.value, datum)

    /** Same as [[updatePolicyStateTx]] but using the reference script (`updatePolicyStateTxRef`). */
    def updatePolicyStateTxRef(
        iid: InsuranceId,
        box: Box[InsurancePolicyDatum],
        state: InsurancePolicyState,
        r: InsurancePolicyRedeemer
    )(using run: Run): TxBuilder =
        import run.given
        val datum = InsurancePolicyDatum.updatePolicyState(box.datum, state).orFail("Can't update policy state")
        run.txBuilder
            .references(insuranceRefScript(iid))
            .spend(box.utxo, r)
            .payTo(insurancePolicyAddress(iid), box.value, datum)

    def runUpdatePolicyState(
        state: InsurancePolicyState,
        r: InsurancePolicyRedeemer,
        iid: InsuranceId,
        signer: User
    )(using run: Run): Unit =
        val tx = updatePolicyStateTx(iid, iinfo(iid), state, r).validFrom(now)
        run.submitTx(signer, tx)

    def triggerPolicyExpiration(iid: InsuranceId, signer: User)(using run: Run): Unit =
        val tx = updatePolicyStateTxRef(iid, iinfo(iid), InsurancePolicyState.Expired, PolicyExpire)
            .validFrom(now)
        run.submitTx(signer, tx)

    // ------------------------------------------------------------------ premium

    /** Pays the premium: moves the policy to Funding, pays each piggy bank its share with a
      * `PBankPremium` datum and returns the premium payment info UTxO to the policy holder.
      */
    def payPremium(iid: InsuranceId, policyHolder: User)(using run: Run): Unit =
        import run.given
        val r = PolicyInitiated(PolicyInitiatedPayPremium)
        val piBox = ppInfo(iid)
        val (amount, piggyBanks) = piBox.datum match
            case PremiumPaymentInfo(amount, piggyBanks) => (amount, piggyBanks.toScalaList)
            case other => throw new IllegalStateException(s"Unexpected payment datum $other")
        val premium: PiggyBankDatum = PBankPremium(amount, 0)
        val tx = piggyBanks.foldLeft(
          updatePolicyStateTxRef(iid, iinfo(iid), InsurancePolicyState.Funding, r)
              .spend(piBox.utxo, r)
              .payTo(policyHolder.address, piBox.value)
        )((b, address) => b.payTo(ledgerAddress(address), Value.lovelace(amount.toLong), premium))
        run.submitTx(policyHolder, tx)

    /** Moves the policy to OnRisk, referencing all piggy banks (`triggerFundingComplete`). */
    def triggerFundingComplete(iid: InsuranceId, users: Users)(using run: Run): Unit =
        import run.given
        val onRiskStartDate = run.currentTime
        run.waitNSlots(5)
        val actualStartTime = run.currentTime
        val box = iinfo(iid)
        val updated = InsurancePolicyDatum.completeFunding(box.datum, onRiskStartDate).orFail("Can't update policy state")
        val piggyBanks = (1 to insuranceInfo(iid).iInfoFidaCardNumber.toInt).flatMap(i =>
            run.boxesAt[PiggyBankDatum](piggyBankAddr(iid, fidaCardFromInt(i)))
                .filter(_.datum.isInstanceOf[PBankFidaCard])
        )
        val tx = piggyBanks.foldLeft(
          run.txBuilder
              .spend(box.utxo, PolicyFunding(PolicyFundingFundingComplete(onRiskStartDate)), insurancePolicy(iid))
              .payTo(insurancePolicyAddress(iid), box.value, updated)
              .validFrom(run.instant(actualStartTime))
        )((b, pb) => b.references(pb.utxo))
        run.submitTx(users.policyHolder, tx)

    // ------------------------------------------------------------------ fida cards

    def buyFidaCard(iid: InsuranceId, investor: User, fcid: FidaCardId)(using run: Run): Unit =
        import run.given
        val box = piggyBankInfo(iid, fcid)
        val card = box.datum match
            case c: PBankFidaCard => c
            case other            => throw new IllegalStateException(s"Unexpected piggy bank datum $other")
        val sold = PiggyBankDatum.setFidaCardSold(box.datum).orFail("Can't buy fida card")
        val tx = run.txBuilder
            .spend(box.utxo, BuyFidaCard, piggyBank(iid, fcid))
            .payTo(
              piggyBankAddr(iid, fcid),
              box.value + Value.lovelace(card.pbfcFidaCardValue.toLong) - fidaCardNFT(iid, fcid),
              sold
            )
            .payTo(investor.address, fidaCardNFT(iid, fcid))
        run.submitTx(investor, tx)

    def buyFidaCards(iid: InsuranceId, investor: User, fcids: Seq[FidaCardId])(using Run): Unit =
        fcids.foreach(buyFidaCard(iid, investor, _))

    def sellFidaCard(iid: InsuranceId, investor: User, fcid: FidaCardId)(using run: Run): Unit =
        import run.given
        val box = piggyBankInfo(iid, fcid)
        val card = box.datum match
            case c: PBankFidaCard => c
            case other            => throw new IllegalStateException(s"Unexpected piggy bank datum $other")
        val unsold = PiggyBankDatum.setFidaCardUnsold(box.datum).orFail("Can't sell fida card")
        val sellValue = Value.lovelace(card.pbfcFidaCardValue.toLong)
        val tx = spendAll(run.txBuilder, fidaCardUtxos(investor, iid, fcid))
            .spend(box.utxo, SellFidaCard, piggyBank(iid, fcid))
            .payTo(piggyBankAddr(iid, fcid), box.value - sellValue + fidaCardNFT(iid, fcid), unsold)
            .payTo(investor.address, sellValue)
        run.submitTx(investor, tx)

    // ------------------------------------------------------------------ premium claims

    /** An investor (holding the fida card) claims `amount` lovelace of unlocked premium. */
    def claimPremium(iid: InsuranceId, fcid: FidaCardId, investor: User, amount: BigInt)(using run: Run): Unit =
        import run.given
        val pbBox = premiumBox(iid, fcid)
        val claimed = Value.lovelace(amount.toLong)
        val base = spendAll(run.txBuilder.references(iinfo(iid).utxo), fidaCardUtxos(investor, iid, fcid))
            .payTo(investor.address, claimed + fidaCardNFT(iid, fcid))
            .spend(pbBox.utxo, ClaimPremium, piggyBank(iid, fcid))
        val tx =
            if pbBox.value == claimed then base
            else base.payTo(piggyBankAddr(iid, fcid), pbBox.value - claimed, pbBox.datum)
        run.submitTx(investor, tx.validFrom(now))

    /** The policy holder claims back `amount` lovelace of premium after cancellation. */
    def claimPremiumOnCancel(iid: InsuranceId, fcid: FidaCardId, policyHolder: User, amount: BigInt)(using
        run: Run
    ): Unit =
        import run.given
        val pbBox = premiumBox(iid, fcid)
        val claimed = Value.lovelace(amount.toLong)
        val updated = PiggyBankDatum.updatePiggyBankRefund(amount, pbBox.datum) match
            case POption.Some(d) => d
            case POption.None    => pbBox.datum
        val base = run.txBuilder
            .references(iinfo(iid).utxo)
            .payTo(policyHolder.address, claimed)
            .spend(pbBox.utxo, ClaimPremiumOnCancel, piggyBank(iid, fcid))
        val tx =
            if pbBox.value == claimed then base
            else base.payTo(piggyBankAddr(iid, fcid), pbBox.value - claimed, updated)
        run.submitTx(policyHolder, tx.validFrom(now))

    // ------------------------------------------------------------------ collateral

    /** The investor pays the accepted claim from the collateral of each piggy bank. */
    def payForClaimWithCollateral(iid: InsuranceId, investor: User)(using run: Run): Unit =
        import run.given
        val iiBox = iinfo(iid)
        val info = insuranceInfo(iid)
        val claim = info.iInfoClaim.orFail("Can't get the current claim")
        for fcid <- fidaCardsFromInts(1 to info.iInfoFidaCardNumber.toInt) do
            val pBox = piggyBankInfo(iid, fcid)
            val payAmount = claim.claimAmount / info.iInfoFidaCardNumber
            val paid = PiggyBankDatum.addPiggyBankPaidClaim(claim.claimId, pBox.datum).orFail("Can't pay for claim")
            val claimPayment: InsurancePolicyDatum = PolicyClaimPayment
            val tx = spendAll(
              run.txBuilder
                  .spend(pBox.utxo, PayForClaimWithCollateral, piggyBank(iid, fcid))
                  .payTo(piggyBankAddr(iid, fcid), pBox.value - Value.lovelace(payAmount.toLong), paid)
                  .payTo(insurancePolicyAddress(iid), Value.lovelace(payAmount.toLong), claimPayment)
                  .references(iiBox.utxo),
              fidaCardUtxos(investor, iid, fcid)
            ).payTo(investor.address, fidaCardNFT(iid, fcid))
            run.submitTx(investor, tx.validFrom(now))

    def unlockCollateralOnExpired(investor: User, iid: InsuranceId, fcid: FidaCardId)(using Run): Unit =
        unlockCollateralOn(UnlockCollateral, investor, iid, fcid)

    def unlockCollateralsOnExpired(investor: User, iid: InsuranceId, fcids: Seq[FidaCardId])(using Run): Unit =
        fcids.foreach(unlockCollateralOnExpired(investor, iid, _))

    def unlockCollateralOnCancel(investor: User, iid: InsuranceId, fcid: FidaCardId)(using Run): Unit =
        unlockCollateralOn(UnlockCollateralOnCancel, investor, iid, fcid)

    /** The card owner takes the whole piggy bank (collateral and status token) back. */
    def unlockCollateralOn(r: PiggyBankRedeemer, investor: User, iid: InsuranceId, fcid: FidaCardId)(using
        run: Run
    ): Unit =
        import run.given
        val pbBox = piggyBankInfo(iid, fcid)
        val tx = spendAll(run.txBuilder.references(iinfo(iid).utxo), fidaCardUtxos(investor, iid, fcid))
            .payTo(investor.address, pbBox.value + fidaCardNFT(iid, fcid))
            .spend(pbBox.utxo, r, piggyBank(iid, fcid))
        run.submitTx(investor, tx)

    // ------------------------------------------------------------------ claims

    val sampleClaimAmount: BigInt = 10_000_000

    /** Funds the policy, sells all cards to investor 1, starts the coverage and files a claim. */
    def createClaim(iid: InsuranceId, policyHolder: User, users: Users)(using run: Run): Unit =
        import run.given
        payPremium(iid, policyHolder)
        buyFidaCards(iid, users.investor1, fidaCardsFromInts(1 to 10))
        triggerFundingComplete(iid, users)
        val claimDate = run.currentTime
        run.waitNSlots(5)
        val claim = ClaimInfo(
          claimAmount = sampleClaimAmount,
          claimDate = claimDate,
          claimReason = ByteString.fromString("I'm sick!"),
          claimAccepted = false,
          claimId = ByteString.fromString("1234")
        )
        updateClaimTx(iid, policyHolder, PolicyOnRiskCreateClaim(claim), validFrom = true)(_ => POption.Some(claim))

    def closeClaim(iid: InsuranceId, policyHolder: User)(using Run): Unit =
        updateClaimTx(iid, policyHolder, PolicyOnRiskCloseClaim, validFrom = false)(_ => POption.None)

    def acceptClaim(iid: InsuranceId, fidaSystem: User)(using Run): Unit =
        updateClaimTx(iid, fidaSystem, PolicyOnRiskAcceptClaim, validFrom = false)(
          _.map(_.copy(claimAccepted = true))
        )

    def expireClaim(iid: InsuranceId, investor: User)(using Run): Unit =
        updateClaimTx(iid, investor, PolicyOnRiskExpireClaim, validFrom = true)(_ => POption.None)

    def failClaim(iid: InsuranceId, fidaSystem: User)(using Run): Unit =
        updateClaimTx(iid, fidaSystem, PolicyOnRiskFailClaim, validFrom = false)(_ => POption.None)

    /** The policy holder collects every claim payment locked at the insurance policy script. */
    def claimPayment(iid: InsuranceId, policyHolder: User)(using run: Run): Unit =
        import run.given
        val payments = run.boxesAt[InsurancePolicyDatum](insurancePolicyAddress(iid))
            .filter(_.datum == PolicyClaimPayment)
        val iiBox = iinfo(iid)
        for box <- payments do
            val tx = run.txBuilder
                .spend(box.utxo, PolicyOnRisk(PolicyOnRiskClaimPayment), insurancePolicy(iid))
                .payTo(policyHolder.address, box.value)
                .references(iiBox.utxo)
            run.submitTx(policyHolder, tx)

    private def updateClaimTx(
        iid: InsuranceId,
        signer: User,
        r: PolicyOnRiskRedeemer,
        validFrom: Boolean
    )(claim: POption[ClaimInfo] => POption[ClaimInfo])(using run: Run): Unit =
        import run.given
        val box = iinfo(iid)
        val current = insuranceInfo(iid).iInfoClaim
        val datum = InsurancePolicyDatum.updateClaim(box.datum, claim(current)).orFail("Can't update claim")
        val tx = run.txBuilder
            .spend(box.utxo, PolicyOnRisk(r), insurancePolicy(iid))
            .payTo(insurancePolicyAddress(iid), box.value, datum)
        run.submitTx(signer, if validFrom then tx.validFrom(now) else tx)
}
