package fida.contract.insurance.piggybank

import fida.contract.insurance.{ClaimInfo, PiggyBankDatum, PiggyBankRedeemer}
import fida.contract.insurance.InsurancePolicyDatum.PolicyClaimPayment
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Expired, OnRisk}
import fida.contract.insurance.PiggyBankDatum.{PBankFidaCard, PBankPremium}
import fida.contract.insurance.PiggyBankRedeemer.*
import fida.contract.testkit.{Generators, Outcome, Shape}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import fida.contract.testkit.Shape.Corruption
import fida.contract.testkit.Shape.Corruption.*
import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.Option
import scalus.cardano.onchain.plutus.v1.{Interval, ScriptPurpose, Value}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{fromData, toData}

/** Properties of the piggy bank validator over every constructor of its datum and redeemer types:
  * it decodes them like PlutusTx `unsafeFromBuiltinData`, the values it decodes are exact, it
  * recognises the datums of other UTxOs like `fromBuiltinData`, and the bounds of its payments.
  */
class PiggyBankPropertiesSpec extends PiggyBankSpecBase with ScalaCheckPropertyChecks {

    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = PosInt(50))

    private val sold = cardDatum(isSold = true)
    private val unsold = cardDatum(isSold = false)
    private val soldValue = value(minAda + fidaCardValue, statusNft)
    private val bothNfts = value(minAda, statusNft, cardNft)

    /** Sells `sold` back, with `outputs` as the continuing outputs. */
    private def sellBack(outputs: (Data, Value)*): ScriptContext =
        spendPiggyBank(
          Seq(piggyBankInput(sold.toData, soldValue), cardOwnerInput),
          outputs = outputs.map(piggyBankOutput(_, _))
        )

    private val sell: Data = SellFidaCard.toData

    /** The context of the decoding properties: the sold card sold back. */
    private val ctx: ScriptContext = sellBack(unsold.toData -> bothNfts)

    private val datums: Seq[Data] = Shape.covering(Shape.piggyBankDatum)
    private val redeemers: Seq[Data] = Shape.covering(Shape.piggyBankRedeemer)

    private val rejectedWithPT1 = Outcome(false, Seq("PT1"))
    private val failedWithoutTrace = Outcome(false, Seq.empty)

    // ---------------------------------------------------------------- decoding

    test("every datum and redeemer constructor decodes: no PT1") {
        for datum <- datums; redeemer <- redeemers do
            withClue(s"datum $datum, redeemer $redeemer") {
                assert(!evalRaw(datum, redeemer, ctx).logs.contains("PT1"))
            }
    }

    test("an unknown constructor anywhere in the datum or the redeemer fails with PT1") {
        for datum <- datums; bad <- Shape.corruptions(Shape.piggyBankDatum, datum, UnknownTag) do
            withClue(s"datum $bad")(assert(evalRaw(bad, sell, ctx) == rejectedWithPT1))
        for
            redeemer <- redeemers
            bad <- Shape.corruptions(Shape.piggyBankRedeemer, redeemer, UnknownTag)
        do withClue(s"redeemer $bad")(assert(evalRaw(sold.toData, bad, ctx) == rejectedWithPT1))
    }

    test("a value of the wrong kind or a missing field fails without a trace") {
        for
            corruption <- Seq(WrongKind, MissingField)
            datum <- datums
            bad <- Shape.corruptions(Shape.piggyBankDatum, datum, corruption)
        do withClue(s"datum $bad")(assert(evalRaw(bad, sell, ctx) == failedWithoutTrace))
        for
            redeemer <- redeemers
            bad <- Shape.corruptions(Shape.piggyBankRedeemer, redeemer, WrongKind)
        do withClue(s"redeemer $bad")(assert(evalRaw(sold.toData, bad, ctx) == failedWithoutTrace))
        for bad <- Shape.otherKinds(Shape.Bytes) do
            withClue(s"parameter $bad") {
                assert(evalRaw(sold.toData, sell, ctx, card = bad) == failedWithoutTrace)
                assert(evalRaw(sold.toData, sell, ctx, insurance = bad) == failedWithoutTrace)
            }
    }

    test("extra constructor fields are ignored") {
        for datum <- datums; extra <- Shape.corruptions(Shape.piggyBankDatum, datum, ExtraField) do
            withClue(s"datum $extra")(
              assert(evalRaw(extra, sell, ctx) == evalRaw(datum, sell, ctx))
            )
        for
            redeemer <- redeemers
            extra <- Shape.corruptions(Shape.piggyBankRedeemer, redeemer, ExtraField)
        do
            withClue(s"redeemer $extra") {
                assert(evalRaw(sold.toData, extra, ctx) == evalRaw(sold.toData, redeemer, ctx))
            }
    }

    test("decoding random data: valid data decodes, malformed data fails as PlutusTx (property)") {
        val datum = Shape.piggyBankDatum
        forAll(Shape.gen(datum), Shape.gen(Shape.piggyBankRedeemer)) { (d, r) =>
            assert(!evalRaw(d, r, ctx).logs.contains("PT1"))
        }
        forAll(genCorrupted(datum, UnknownTag))(bad =>
            assert(evalRaw(bad, sell, ctx) == rejectedWithPT1)
        )
        forAll(Gen.oneOf(WrongKind, MissingField).flatMap(genCorrupted(datum, _))) { bad =>
            assert(evalRaw(bad, sell, ctx) == failedWithoutTrace)
        }
    }

    /** Valid data of `shape` with `corruption` applied at a random node where it applies. */
    private def genCorrupted(shape: Shape, corruption: Corruption): Gen[Data] =
        Shape
            .gen(shape)
            .map(Shape.corruptions(shape, _, corruption))
            .suchThat(_.nonEmpty)
            .flatMap(cases => Gen.oneOf(cases))

    // ---------------------------------------------------------------- decoded values

    /** Buys `card` (unsold), paying exactly its value into the status output. */
    private def buy(card: PBankFidaCard, input: Data, paid: BigInt = 0): Outcome =
        val soldCard = card.copy(pbfcIsSold = true)
        evalRaw(
          input,
          BuyFidaCard.toData,
          spendPiggyBank(
            Seq(piggyBankInput(card.toData, bothNfts), walletInput),
            outputs = Seq(
              piggyBankOutput(
                soldCard.toData,
                value(minAda + card.pbfcFidaCardValue + paid, statusNft)
              )
            )
          )
        )

    private def isFidaCard(d: Data): Boolean = d match
        case Data.Constr(tag, _) => tag == 1
        case _                   => false

    private def fidaCard(d: Data): PBankFidaCard = fromData[PiggyBankDatum](d) match
        case card: PBankFidaCard => card.copy(pbfcIsSold = false)
        case other               => fail(s"not a fida card: $other")

    test("BuyFidaCard re-encodes the decoded fida card exactly, extra fields dropped") {
        for d <- datums.filter(isFidaCard) do
            val card = fidaCard(d)
            for input <- card.toData +: Shape.corruptions(
                  Shape.piggyBankDatum,
                  card.toData,
                  ExtraField
                )
            do withClue(s"datum $input")(assertSucceeds(buy(card, input)))
    }

    test("BuyFidaCard re-encodes any decoded fida card exactly (property)") {
        forAll(Generators.genPiggyBankDatum.map(_.toData).suchThat(isFidaCard))(d =>
            assertSucceeds(buy(fidaCard(d), fidaCard(d).toData))
        )
    }

    test(
      "BuyFidaCard: the status output holds at least the card value on top of the input (property)"
    ) {
        val card = fidaCard(unsold.toData)
        forAll(Gen.choose(BigInt(0), BigInt(10_000_000_000L))) { extra =>
            assertSucceeds(buy(card, card.toData, paid = extra))
        }
        forAll(Gen.choose(BigInt(1), fidaCardValue)) { missing =>
            assertRejectedWith("ERR-PBANK-4")(buy(card, card.toData, paid = -missing))
        }
    }

    // ---------------------------------------------------------------- other UTxOs' datums

    test("SellFidaCard counts a continuing output only with an exact PiggyBankDatum") {
        def withCandidate(candidate: Data) =
            evalRaw(sold.toData, sell, sellBack(unsold.toData -> bothNfts, candidate -> bothNfts))
        for d <- datums do
            withClue(s"candidate $d")(assertRejectedWith("ERR-PBANK-8")(withCandidate(d)))
            for
                corruption <- Corruption.values.toSeq
                bad <- Shape.corruptions(Shape.piggyBankDatum, d, corruption)
            do withClue(s"candidate $bad")(assertSucceeds(withCandidate(bad)))
    }

    test("SellFidaCard counts a continuing output only with both the card and the status NFT") {
        for nfts <- Seq(value(minAda, cardNft), value(minAda, statusNft)) do
            withClue(s"value $nfts") {
                assertRejectedWith("ERR-PBANK-8")(
                  evalRaw(sold.toData, sell, sellBack(unsold.toData -> nfts))
                )
            }
    }

    private val collateral = value(minAda + fidaCardValue, statusNft)

    /** The card owner takes the collateral back, with policy info reference inputs holding
      * `policyInfos`.
      */
    private def unlockWith(redeemer: PiggyBankRedeemer, policyInfos: Data*): Outcome =
        evalRaw(
          sold.toData,
          redeemer.toData,
          spendPiggyBank(
            Seq(piggyBankInput(sold.toData, collateral), cardOwnerInput),
            refs = policyInfos.zipWithIndex.map((d, i) => policyInfoRef(d, n = 1 + i)),
            outputs = Seq(txOutNoDatum(investorAddress, collateral + value(0, cardNft)))
          )
        )

    test("UnlockCollateral counts policy info only with an exact InsuranceInfo datum") {
        val expired = sampleInsuranceInfo(state = Expired, startDate = Option.Some(beginningOfTime))
        assertSucceeds(unlockWith(UnlockCollateral, expired.toData))
        for d <- Shape.covering(Shape.insuranceInfo) do
            withClue(s"candidate $d") {
                assertRejectedWith("ERR-PBANK-31")(unlockWith(UnlockCollateral, expired.toData, d))
            }
            for
                corruption <- Corruption.values.toSeq
                bad <- Shape.corruptions(Shape.insuranceInfo, d, corruption)
            do
                withClue(s"candidate $bad") {
                    assertSucceeds(unlockWith(UnlockCollateral, expired.toData, bad))
                }
    }

    private val acceptedClaim: ClaimInfo = sampleClaim.copy(claimAccepted = true)
    private val onRiskWithClaim = sampleInsuranceInfo(
      state = OnRisk,
      startDate = Option.Some(beginningOfTime),
      claim = Option.Some(acceptedClaim)
    )

    /** Each piggy bank pays its share of the claim: 10 ADA / 10 cards. */
    private val share: BigInt = acceptedClaim.claimAmount / fidaCardQuantity

    /** The card owner pays `paid` to the insurance, taking `withdrawn` out of the collateral. */
    private def payForClaim(
        policyInfos: Seq[Data] = Seq(onRiskWithClaim.toData),
        withdrawn: BigInt = share,
        paid: BigInt = share,
        otherRefs: Seq[TxInInfo] = Seq.empty
    ): Outcome =
        val inputCollateral = minAda + fidaCardValue
        val paidCard = cardDatum(isSold = true, paidClaims = Seq(acceptedClaim.claimId))
        evalRaw(
          sold.toData,
          PayForClaimWithCollateral.toData,
          spendPiggyBank(
            Seq(piggyBankInput(sold.toData, value(inputCollateral, statusNft)), cardOwnerInput),
            refs = policyInfos.zipWithIndex.map((d, i) => policyInfoRef(d, n = 1 + i)) ++ otherRefs,
            outputs = Seq(
              piggyBankOutput(paidCard.toData, value(inputCollateral - withdrawn, statusNft)),
              txOut(policyAddress, value(paid), PolicyClaimPayment.toData)
            )
          )
        )

    test("PayForClaimWithCollateral counts policy info only with an exact policy datum") {
        assertSucceeds(payForClaim())
        for d <- Shape.covering(Shape.insurancePolicyDatum) do
            withClue(s"candidate $d") {
                assertRejectedWith("ERR-PBANK-18")(payForClaim(Seq(onRiskWithClaim.toData, d)))
            }
            for
                corruption <- Corruption.values.toSeq
                bad <- Shape.corruptions(Shape.insurancePolicyDatum, d, corruption)
            do
                withClue(s"candidate $bad") {
                    assertSucceeds(payForClaim(Seq(onRiskWithClaim.toData, bad)))
                }
    }

    test("PayForClaimWithCollateral ignores policy info NFTs without an inline datum") {
        val noDatum = txIn(outRef(9), txOutNoDatum(policyAddress, value(minAda, infoNft)))
        assertSucceeds(payForClaim(otherRefs = Seq(noDatum)))
    }

    test("PayForClaimWithCollateral needs the policy info to be an InsuranceInfo (ERR-PBANK-18)") {
        for d <- PolicyClaimPayment.toData +: Shape.covering(Shape.premiumPaymentInfo) do
            withClue(s"policy info $d")(assertRejectedWith("ERR-PBANK-18")(payForClaim(Seq(d))))
    }

    test(
      "PayForClaimWithCollateral: at most the card's share of the claim leaves the collateral (property)"
    ) {
        forAll(Gen.choose(BigInt(0), share))(w => assertSucceeds(payForClaim(withdrawn = w)))
        forAll(Gen.choose(share + 1, fidaCardValue)) { w =>
            assertRejectedWith("ERR-PBANK-12")(payForClaim(withdrawn = w))
        }
    }

    test("PayForClaimWithCollateral: at least the card's share of the claim is paid (property)") {
        forAll(Gen.choose(share, 10 * share))(p => assertSucceeds(payForClaim(paid = p)))
        forAll(Gen.choose(BigInt(0), share - 1)) { p =>
            assertRejectedWith("ERR-PBANK-15")(payForClaim(paid = p))
        }
    }

    // ---------------------------------------------------------------- premium outputs

    private val premiumShare: BigInt = policyPrice / fidaCardQuantity
    private val installment: BigInt = premiumShare / 4
    private val premium = PBankPremium(premiumShare, 0)
    private val afterFirstInstallment = Interval.after(beginningOfTime + days(90) + 1)

    test("ClaimPremium counts as locked only the continuing outputs with the premium datum") {
        val onRisk = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(beginningOfTime))
        def claimLeaving(lockedOutputDatum: scala.Option[Data]) =
            val locked = premiumShare - installment
            val lockedOutput = lockedOutputDatum match
                case Some(d) => piggyBankOutput(d, value(locked))
                case None    => txOutNoDatum(piggyBankAddress, value(locked))
            eval(
              premium,
              ClaimPremium,
              spendPiggyBank(
                Seq(piggyBankInput(premium.toData, value(premiumShare)), cardOwnerInput),
                refs = Seq(policyInfoRef(onRisk.toData)),
                outputs =
                    Seq(lockedOutput, txOutNoDatum(investorAddress, value(installment, cardNft))),
                validRange = afterFirstInstallment
              )
            )
        assertSucceeds(claimLeaving(Some(premium.toData)))
        assertRejectedWith("ERR-PBANK-8")(claimLeaving(None))
        for extra <- Shape.corruptions(Shape.piggyBankDatum, premium.toData, ExtraField) do
            withClue(s"input datum $extra") {
                assert(
                  evalRaw(
                    extra,
                    ClaimPremium.toData,
                    spendPiggyBank(
                      Seq(piggyBankInput(premium.toData, value(premiumShare)), cardOwnerInput),
                      refs = Seq(policyInfoRef(onRisk.toData)),
                      outputs = Seq(
                        piggyBankOutput(premium.toData, value(premiumShare - installment)),
                        txOutNoDatum(investorAddress, value(installment, cardNft))
                      ),
                      validRange = afterFirstInstallment
                    )
                  ).isSuccess
                )
            }
    }

    test(
      "ClaimPremiumOnCancel counts as locked only the continuing outputs with the refunded datum"
    ) {
        val cancelled =
            sampleInsuranceInfo(state = Cancelled, startDate = Option.Some(beginningOfTime))
        val refund = premiumShare - installment
        def refundLeaving(lockedOutputDatum: scala.Option[Data]) =
            val locked = premiumShare - refund
            val lockedOutput = lockedOutputDatum match
                case Some(d) => piggyBankOutput(d, value(locked))
                case None    => txOutNoDatum(piggyBankAddress, value(locked))
            eval(
              premium,
              ClaimPremiumOnCancel,
              spendPiggyBank(
                Seq(piggyBankInput(premium.toData, value(premiumShare))),
                refs = Seq(policyInfoRef(cancelled.toData)),
                outputs =
                    Seq(lockedOutput, txOutNoDatum(pubKeyAddress(policyHolder), value(refund))),
                validRange = afterFirstInstallment,
                signatories = Seq(policyHolder)
              )
            )
        assertSucceeds(refundLeaving(Some(PBankPremium(premiumShare, refund).toData)))
        assertRejectedWith("ERR-PBANK-23")(refundLeaving(None))
    }

    test("ClaimPremiumOnCancel outside a spending context fails looking for its outputs (Lf)") {
        val cancelled =
            sampleInsuranceInfo(state = Cancelled, startDate = Option.Some(beginningOfTime))
        val minting = ScriptContext(
          txInfo(
            referenceInputs = Seq(policyInfoRef(cancelled.toData)),
            signatories = Seq(policyHolder),
            validRange = afterFirstInstallment
          ),
          ScriptPurpose.Minting(cs)
        )
        assert(eval(premium, ClaimPremiumOnCancel, minting) == Outcome(false, Seq("Lf")))
    }
}
