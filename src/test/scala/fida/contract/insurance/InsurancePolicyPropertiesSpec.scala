package fida.contract.insurance

import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PremiumPaymentInfo}
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PiggyBankDatum.PBankPremium
import fida.contract.insurance.PolicyInitiatedRedeemer.PolicyInitiatedPayPremium
import fida.contract.testkit.{
    Generators,
    OnChainScript,
    Outcome,
    ScriptAssertions,
    ScriptEval,
    Shape
}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import fida.contract.testkit.Shape.Corruption
import fida.contract.testkit.Shape.Corruption.*
import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option}
import scalus.cardano.onchain.plutus.v1.{Address, Interval, PosixTime}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxOut}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{fromData, toData}

/** Properties of the insurance policy validator over every constructor of its datum and redeemer
  * types: it decodes them like PlutusTx `unsafeFromBuiltinData` (an unknown constructor fails with
  * `PT1`, other malformed data fails without a trace, extra fields are ignored), the values it
  * decodes are exact, and it recognises the datums of other outputs like `fromBuiltinData`.
  */
class InsurancePolicyPropertiesSpec
    extends AnyFunSuite
    with ScriptAssertions
    with ScalaCheckPropertyChecks {

    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = PosInt(50))

    private val policyAddress = scriptAddress(scriptHash(1))
    private val policyRef = outRef(1)
    private val infoNft = (insuranceId.cs, Tokens.policyInfoTokenName, BigInt(1))
    private val paymentNft = (insuranceId.cs, Tokens.policyPaymentTokenName, BigInt(1))

    private def run(
        datum: Data,
        redeemer: Data,
        ctx: ScriptContext,
        iid: Data = insuranceId.toData
    ): Outcome =
        ScriptEval.eval(OnChainScript.insurancePolicyValidator, iid, datum, redeemer, ctx.toData)

    /** Spends the policy info UTxO holding `datum` and recreates it with `outDatum`. */
    private def expireCtx(datum: Data, outDatum: Data, validFrom: PosixTime): ScriptContext =
        val in = txIn(policyRef, txOut(policyAddress, value(2_000_000, infoNft), datum))
        spending(
          txInfo(
            inputs = Seq(in),
            outputs = Seq(txOut(policyAddress, value(2_000_000, infoNft), outDatum)),
            validRange = Interval.after(validFrom)
          ),
          policyRef
        )

    private val sample: InsuranceInfo = sampleInsuranceInfo(state = Funding)
    private val expire: Data = PolicyExpire.toData

    /** The context of the decoding properties: the sample policy expiring. */
    private val ctx: ScriptContext = expireCtx(
      sample.toData,
      sample.copy(iInfoState = Expired).toData,
      sample.iInfoFundingDeadline + 1
    )

    private val datums: Seq[Data] = Shape.covering(Shape.insurancePolicyDatum)
    private val redeemers: Seq[Data] = Shape.covering(Shape.insurancePolicyRedeemer)

    private val rejectedWithPT1 = Outcome(false, Seq("PT1"))
    private val failedWithoutTrace = Outcome(false, Seq.empty)

    // ---------------------------------------------------------------- decoding

    test("every datum and redeemer constructor decodes: no PT1, no failure without a trace") {
        for datum <- datums; redeemer <- redeemers do
            val outcome = run(datum, redeemer, ctx)
            withClue(s"datum $datum, redeemer $redeemer: $outcome") {
                assert(!outcome.logs.contains("PT1"))
                assert(outcome.isSuccess || outcome.logs.nonEmpty)
            }
    }

    test("an unknown constructor anywhere in the datum or the redeemer fails with PT1") {
        for
            datum <- datums
            bad <- Shape.corruptions(Shape.insurancePolicyDatum, datum, UnknownTag)
        do withClue(s"datum $bad")(assert(run(bad, expire, ctx) == rejectedWithPT1))
        for
            redeemer <- redeemers
            bad <- Shape.corruptions(Shape.insurancePolicyRedeemer, redeemer, UnknownTag)
        do withClue(s"redeemer $bad")(assert(run(sample.toData, bad, ctx) == rejectedWithPT1))
    }

    test("a value of the wrong kind or a missing field fails without a trace") {
        for
            corruption <- Seq(WrongKind, MissingField)
            datum <- datums
            bad <- Shape.corruptions(Shape.insurancePolicyDatum, datum, corruption)
        do withClue(s"datum $bad")(assert(run(bad, expire, ctx) == failedWithoutTrace))
        for
            corruption <- Seq(WrongKind, MissingField)
            redeemer <- redeemers
            bad <- Shape.corruptions(Shape.insurancePolicyRedeemer, redeemer, corruption)
        do withClue(s"redeemer $bad")(assert(run(sample.toData, bad, ctx) == failedWithoutTrace))
        for bad <- Shape.otherKinds(Shape.Bytes) do
            withClue(s"insurance id $bad") {
                assert(run(sample.toData, expire, ctx, iid = bad) == failedWithoutTrace)
            }
    }

    test("extra constructor fields are ignored") {
        for
            datum <- datums
            extra <- Shape.corruptions(Shape.insurancePolicyDatum, datum, ExtraField)
        do withClue(s"datum $extra")(assert(run(extra, expire, ctx) == run(datum, expire, ctx)))
        for
            redeemer <- redeemers
            extra <- Shape.corruptions(Shape.insurancePolicyRedeemer, redeemer, ExtraField)
        do
            withClue(s"redeemer $extra") {
                assert(run(sample.toData, extra, ctx) == run(sample.toData, redeemer, ctx))
            }
    }

    test("decoding random data: valid data decodes, malformed data fails as PlutusTx (property)") {
        val datum = Shape.insurancePolicyDatum
        val redeemer = Shape.insurancePolicyRedeemer
        forAll(Shape.gen(datum), Shape.gen(redeemer)) { (d, r) =>
            val outcome = run(d, r, ctx)
            assert(!outcome.logs.contains("PT1"))
            assert(outcome.isSuccess || outcome.logs.nonEmpty)
        }
        forAll(genCorrupted(datum, UnknownTag))(bad =>
            assert(run(bad, expire, ctx) == rejectedWithPT1)
        )
        forAll(genCorrupted(redeemer, UnknownTag)) { bad =>
            assert(run(sample.toData, bad, ctx) == rejectedWithPT1)
        }
        forAll(Gen.oneOf(WrongKind, MissingField).flatMap(genCorrupted(datum, _))) { bad =>
            assert(run(bad, expire, ctx) == failedWithoutTrace)
        }
        forAll(
          Shape
              .gen(datum)
              .flatMap(d => genOneOf(Shape.corruptions(datum, d, ExtraField)).map((d, _)))
        ) { (d, extra) =>
            assert(run(extra, expire, ctx) == run(d, expire, ctx))
        }
    }

    /** Valid data of `shape` with `corruption` applied at a random node where it applies. */
    private def genCorrupted(shape: Shape, corruption: Corruption): Gen[Data] =
        Shape
            .gen(shape)
            .map(Shape.corruptions(shape, _, corruption))
            .suchThat(_.nonEmpty)
            .flatMap(genOneOf)

    private def genOneOf(cases: Seq[Data]): Gen[Data] = Gen.oneOf(cases)

    // ---------------------------------------------------------------- decoded values

    private def insuranceInfo(d: Data): InsuranceInfo = fromData[InsurancePolicyDatum](d) match
        case info: InsuranceInfo => info
        case other               => fail(s"not an InsuranceInfo: $other")

    /** PolicyExpire passes only when the output datum is the input datum, decoded and re-encoded in
      * state Expired: every field must decode to its exact value.
      */
    private def assertExpires(funding: InsuranceInfo, inputs: Seq[Data], validFrom: PosixTime) =
        val expiring =
            expireCtx(funding.toData, funding.copy(iInfoState = Expired).toData, validFrom)
        for input <- inputs do
            withClue(s"datum $input")(assertSucceeds(run(input, expire, expiring)))

    test("PolicyExpire re-encodes the decoded policy info exactly, extra fields dropped") {
        for d <- Shape.covering(Shape.insuranceInfo) do
            val funding: InsuranceInfo =
                insuranceInfo(d).copy(iInfoState = Funding, iInfoFundingDeadline = beginningOfTime)
            val withExtraFields = Shape.corruptions(Shape.insuranceInfo, funding.toData, ExtraField)
            assertExpires(funding, funding.toData +: withExtraFields, beginningOfTime + 1)
    }

    test("PolicyExpire re-encodes any decoded policy info exactly (property)") {
        forAll(Generators.genInsuranceInfo, Gen.choose(BigInt(1), days(1000))) { (info, delay) =>
            val funding: InsuranceInfo = info.copy(iInfoState = Funding)
            assertExpires(funding, Seq(funding.toData), funding.iInfoFundingDeadline + delay)
        }
        forAll(Generators.genInsuranceInfo, Generators.genInteger, Gen.choose(BigInt(1), days(9))) {
            (info, start, delay) =>
                val onRisk: InsuranceInfo =
                    info.copy(iInfoState = OnRisk, iInfoStartDate = Option.Some(start))
                val expiring = expireCtx(
                  onRisk.toData,
                  onRisk.copy(iInfoState = Expired).toData,
                  start + onRisk.iInfoInsurancePeriod + delay
                )
                assertSucceeds(run(onRisk.toData, expire, expiring))
        }
    }

    test("PolicyExpire needs a funding or on-risk policy, with a start date or not (ERR-MAIN-1)") {
        for
            state <- Seq(Initiated, Expired)
            start <- Seq(Option.None, Option.Some(beginningOfTime))
        do
            val info: InsuranceInfo = sampleInsuranceInfo(state = state, startDate = start)
            val late = expireCtx(
              info.toData,
              info.copy(iInfoState = Expired).toData,
              beginningOfTime + days(1000)
            )
            withClue(s"$state, start $start")(
              assertFailsWith("ERR-MAIN-1")(run(info.toData, expire, late))
            )
    }

    private val payPremiumRedeemer: Data = PolicyInitiated(PolicyInitiatedPayPremium).toData

    /** Spends the premium payment info UTxO holding `paymentInfo` with `outputs` and the policy
      * info output.
      */
    private def payPremium(paymentInfo: Data, outputs: Seq[TxOut]): ScriptContext =
        val paymentRef = outRef(2)
        val in = txIn(paymentRef, txOut(policyAddress, value(2_000_000, paymentNft), paymentInfo))
        val policyInfo =
            txOut(policyAddress, value(2_000_000, infoNft), sampleInsuranceInfo().toData)
        spending(txInfo(inputs = Seq(in), outputs = outputs :+ policyInfo), paymentRef)

    private def premiumPayment(address: Address, premium: BigInt): TxOut =
        txOut(address, value(premium), PBankPremium(premium, 0).toData)

    test("PayPremium decodes every kind of piggy bank address exactly, extra fields dropped") {
        val premium = BigInt(20_000_000)
        val addresses = Shape.covering(Shape.address).map(fromData[Address])
        val info: InsurancePolicyDatum = PremiumPaymentInfo(premium, PList.from(addresses))
        val ctx = payPremium(info.toData, addresses.map(premiumPayment(_, premium)))
        val withExtraFields = Shape.corruptions(Shape.premiumPaymentInfo, info.toData, ExtraField)
        for input <- info.toData +: withExtraFields do
            withClue(s"datum $input")(assertSucceeds(run(input, payPremiumRedeemer, ctx)))
    }

    test("PayPremium decodes any piggy bank addresses exactly (property)") {
        val genAddresses = Gen.choose(1, 5).flatMap(Gen.listOfN(_, Generators.genAddress))
        forAll(genAddresses.map(_.distinct), Gen.choose(BigInt(1), BigInt(100_000_000))) {
            (addresses, premium) =>
                val info: InsurancePolicyDatum = PremiumPaymentInfo(premium, PList.from(addresses))
                val ctx = payPremium(info.toData, addresses.map(premiumPayment(_, premium)))
                assertSucceeds(run(info.toData, payPremiumRedeemer, ctx))
        }
    }

    // ---------------------------------------------------------------- other outputs' datums

    test("PayPremium counts a payment only with an exact PBankPremium datum (fromBuiltinData)") {
        val premium = BigInt(20_000_000)
        val (a, b) = (pubKeyAddress(pkh(31)), scriptAddress(scriptHash(32)))
        val info: InsurancePolicyDatum = PremiumPaymentInfo(premium, PList(a, b))
        val exact = PBankPremium(premium, 0).toData
        def payingB(datum: Data) = run(
          info.toData,
          payPremiumRedeemer,
          payPremium(info.toData, Seq(premiumPayment(a, premium), txOut(b, value(premium), datum)))
        )
        assertSucceeds(payingB(exact))
        for
            corruption <- Corruption.values.toSeq
            bad <- Shape.corruptions(Shape.pBankPremium, exact, corruption)
        do withClue(s"datum $bad")(assertFailsWith("ERR-INIT-3")(payingB(bad)))
    }

    test("an output holding the policy info NFT without an inline datum is not the policy info") {
        val in = txIn(policyRef, txOut(policyAddress, value(2_000_000, infoNft), sample.toData))
        val noDatum = spending(
          txInfo(
            inputs = Seq(in),
            outputs = Seq(txOutNoDatum(policyAddress, value(2_000_000, infoNft))),
            validRange = Interval.after(sample.iInfoFundingDeadline + 1)
          ),
          policyRef
        )
        assertFailsWith("ERR-MAIN-3")(run(sample.toData, expire, noDatum))
    }

    test("a failed validation ends its trace with PT5, like PlutusTx check") {
        val atDeadline = expireCtx(
          sample.toData,
          sample.copy(iInfoState = Expired).toData,
          sample.iInfoFundingDeadline
        )
        assert(run(sample.toData, expire, atDeadline) == Outcome(false, Seq("ERR-MAIN-2", "PT5")))
    }
}
