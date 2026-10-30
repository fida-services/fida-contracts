package fida.contract.insurance

import fida.contract.Scripts
import fida.contract.insurance.InsurancePolicyDatum.PolicyClaimPayment
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.testkit.{ScriptAssertions, ScriptEval}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.Result
import scalus.cardano.onchain.plutus.v1.{Interval, PosixTime}
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option}

/** Direct-evaluation tests of the main insurance policy dispatch and the expiry action. */
class InsurancePolicyValidatorSpec extends AnyFunSuite with ScriptAssertions {

    private val policyAddress = scriptAddress(scriptHash(1))
    private val policyRef = outRef(1)
    private val infoNft = (insuranceId.cs, Tokens.policyInfoTokenName, BigInt(1))

    private def run(
        datum: InsurancePolicyDatum,
        redeemer: InsurancePolicyRedeemer,
        ctx: ScriptContext
    ): Result =
        ScriptEval.eval(
          Scripts.insurancePolicyValidator.program,
          insuranceId.toData,
          datum.toData,
          redeemer.toData,
          ctx.toData
        )

    /** Spends the policy info UTxO holding `datum` and recreates it with `outDatum`. */
    private def expireCtx(
        datum: InsurancePolicyDatum,
        outDatum: Data,
        validFrom: PosixTime
    ): ScriptContext =
        val in = txIn(policyRef, txOut(policyAddress, value(2_000_000, infoNft), datum.toData))
        spending(
          txInfo(
            inputs = Seq(in),
            outputs = Seq(txOut(policyAddress, value(2_000_000, infoNft), outDatum)),
            validRange = Interval.after(validFrom)
          ),
          policyRef
        )

    test("a cancelled policy rejects every non-Initiated action (ERR-CANC-0)") {
        val datum = sampleInsuranceInfo(state = Cancelled)
        val ctx = expireCtx(datum, datum.toData, beginningOfTime + days(400))
        for r <- Seq(PolicyExpire, PolicyFunding(PolicyFundingRedeemer.PolicyFundingCancel)) do
            assertFailsWith("ERR-CANC-0")(run(datum, r, ctx))
    }

    test("expiry is rejected for a datum that is not InsuranceInfo (ERR-MAIN-0)") {
        val ctx = expireCtx(PolicyClaimPayment, PolicyClaimPayment.toData, beginningOfTime)
        assertFailsWith("ERR-MAIN-0")(run(PolicyClaimPayment, PolicyExpire, ctx))
    }

    test("a funding policy expires after its funding deadline") {
        val datum = sampleInsuranceInfo(state = Funding)
        val expired = sampleInsuranceInfo(state = Expired)
        assertSucceeds(
          run(datum, PolicyExpire, expireCtx(datum, expired.toData, datum.iInfoFundingDeadline + 1))
        )
    }

    test("a funding policy cannot expire at its funding deadline (ERR-MAIN-2)") {
        val datum = sampleInsuranceInfo(state = Funding)
        val expired = sampleInsuranceInfo(state = Expired)
        assertFailsWith("ERR-MAIN-2")(
          run(datum, PolicyExpire, expireCtx(datum, expired.toData, datum.iInfoFundingDeadline))
        )
    }

    test("an on-risk policy expires after start date plus insurance period") {
        val start = beginningOfTime + days(3)
        val datum = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(start))
        val expired = sampleInsuranceInfo(state = Expired, startDate = Option.Some(start))
        assertSucceeds(
          run(datum, PolicyExpire, expireCtx(datum, expired.toData, start + days(365) + 1))
        )
        assertFailsWith("ERR-MAIN-2")(
          run(datum, PolicyExpire, expireCtx(datum, expired.toData, start + days(364)))
        )
    }

    test("expiry requires the Expired state in the output datum (ERR-MAIN-3)") {
        val start = beginningOfTime
        val datum = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(start))
        val wrong = sampleInsuranceInfo(state = Cancelled, startDate = Option.Some(start))
        assertFailsWith("ERR-MAIN-3")(
          run(datum, PolicyExpire, expireCtx(datum, wrong.toData, start + days(366)))
        )
    }

    // ---------------------------------------------------------------- strict decoding
    // The datum and redeemer are decoded like PlutusTx `unsafeFromBuiltinData`: an unknown
    // constructor fails with `PT1`, extra fields are ignored, and the decoded values are re-encoded
    // canonically.

    private def evalRaw(datum: Data, redeemer: Data, ctx: ScriptContext): Result =
        ScriptEval.eval(
          Scripts.insurancePolicyValidator.program,
          insuranceId.toData,
          datum,
          redeemer,
          ctx.toData
        )

    /** Spends the policy info UTxO holding raw `input` and recreates it with `output`. */
    private def rawCtx(
        input: Data,
        output: Data,
        validFrom: PosixTime,
        signers: Seq[scalus.cardano.onchain.plutus.v1.PubKeyHash] = Seq.empty
    ): ScriptContext =
        val in = txIn(policyRef, txOut(policyAddress, value(2_000_000, infoNft), input))
        spending(
          txInfo(
            inputs = Seq(in),
            outputs = Seq(txOut(policyAddress, value(2_000_000, infoNft), output)),
            validRange = Interval.after(validFrom),
            signatories = signers
          ),
          policyRef
        )

    private def withField(datum: Data, index: Int, field: Data): Data = datum match
        case Data.Constr(tag, fields) =>
            Data.Constr(tag, PList.from(fields.toScalaList.updated(index, field)))
        case other => fail(s"not a constructor: $other")

    private def withExtraField(datum: Data, field: Data): Data = datum match
        case Data.Constr(tag, fields) => Data.Constr(tag, PList.from(fields.toScalaList :+ field))
        case other                    => fail(s"not a constructor: $other")

    private val expire: Data = (PolicyExpire: InsurancePolicyRedeemer).toData

    test("unchanged fields are re-encoded canonically in the expected output datum") {
        val nonCanonicalNothing = Data.Constr(1, PList.from(Seq(Data.I(0))))
        val datum = sampleInsuranceInfo(state = Funding)
        val input = withField(datum.toData, 14, nonCanonicalNothing)
        val expired = sampleInsuranceInfo(state = Expired).toData
        val after = datum.iInfoFundingDeadline + 1
        assertSucceeds(evalRaw(input, expire, rawCtx(input, expired, after)))
        assertFailsWith("ERR-MAIN-3")(
          evalRaw(input, expire, rawCtx(input, withField(expired, 14, nonCanonicalNothing), after))
        )
    }

    test("extra datum fields are ignored and dropped from the expected output datum") {
        val datum = sampleInsuranceInfo(state = Funding)
        val input = withExtraField(datum.toData, Data.I(42))
        val expired = sampleInsuranceInfo(state = Expired).toData
        val after = datum.iInfoFundingDeadline + 1
        assertSucceeds(evalRaw(input, expire, rawCtx(input, expired, after)))
        assertFailsWith("ERR-MAIN-3")(
          evalRaw(input, expire, rawCtx(input, withExtraField(expired, Data.I(42)), after))
        )
    }

    test("a claim filed with an extra field is stored canonically") {
        val start = beginningOfTime
        val datum = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(start))
        val claim = sampleClaim.copy(claimDate = start + days(1))
        val claimWithExtra = withExtraField(claim.toData, Data.I(0))
        val redeemer =
            Data.Constr(2, PList.from(Seq(Data.Constr(0, PList.from(Seq(claimWithExtra))))))
        val canonical = sampleInsuranceInfo(
          state = OnRisk,
          startDate = Option.Some(start),
          claim = Option.Some(claim)
        ).toData
        val smuggled = withField(datum.toData, 14, Data.Constr(0, PList.from(Seq(claimWithExtra))))
        val validFrom = start + days(2)
        assertSucceeds(
          evalRaw(
            datum.toData,
            redeemer,
            rawCtx(datum.toData, canonical, validFrom, Seq(policyHolder))
          )
        )
        assertFailsWith("ERR-ON-RISK-0")(
          evalRaw(
            datum.toData,
            redeemer,
            rawCtx(datum.toData, smuggled, validFrom, Seq(policyHolder))
          )
        )
    }

    test("a malformed claim in the redeemer fails the script") {
        val start = beginningOfTime
        val datum = sampleInsuranceInfo(state = OnRisk, startDate = Option.Some(start))
        val claim = sampleClaim.copy(claimDate = start + days(1))
        val badAmount = withField(claim.toData, 0, Data.B(scalus.uplc.builtin.ByteString.empty))
        val redeemer = Data.Constr(2, PList.from(Seq(Data.Constr(0, PList.from(Seq(badAmount))))))
        val output = withField(datum.toData, 14, Data.Constr(0, PList.from(Seq(badAmount))))
        val result = evalRaw(
          datum.toData,
          redeemer,
          rawCtx(datum.toData, output, start + days(2), Seq(policyHolder))
        )
        assert(result.isFailure, s"expected failure, got $result")
        assert(
          !result.logs.exists(_.startsWith("ERR-")),
          s"expected a decoding failure, got ${result.logs}"
        )
    }

    test("a non-canonical Cancelled state is still a cancelled policy (ERR-CANC-0)") {
        val datum = sampleInsuranceInfo(state = Funding)
        val cancelledWithField =
            withField(datum.toData, 8, Data.Constr(3, PList.from(Seq(Data.I(0)))))
        val funding = (PolicyFunding(
          PolicyFundingRedeemer.PolicyFundingCancel
        ): InsurancePolicyRedeemer).toData
        val output = sampleInsuranceInfo(state = Cancelled).toData
        assertFailsWith("ERR-CANC-0")(
          evalRaw(
            cancelledWithField,
            funding,
            rawCtx(cancelledWithField, output, beginningOfTime, Seq(broker1))
          )
        )
    }

    test("unknown redeemer constructors fail with PT1") {
        val datum = sampleInsuranceInfo(state = Funding)
        val ctx = rawCtx(
          datum.toData,
          sampleInsuranceInfo(state = OnRisk).toData,
          beginningOfTime,
          Seq(policyHolder)
        )
        val unknown = Seq(
          Data.Constr(4, PList.Nil),
          Data.Constr(0, PList.from(Seq(Data.Constr(2, PList.Nil)))),
          Data.Constr(1, PList.from(Seq(Data.Constr(2, PList.from(Seq(Data.I(0))))))),
          Data.Constr(2, PList.from(Seq(Data.Constr(8, PList.Nil))))
        )
        for redeemer <- unknown do assertFailsWith("PT1")(evalRaw(datum.toData, redeemer, ctx))
    }

    test("unknown datum constructors fail with PT1") {
        val datum = Data.Constr(3, PList.Nil)
        assertFailsWith("PT1")(evalRaw(datum, expire, rawCtx(datum, datum, beginningOfTime)))
        val unknownState =
            withField(sampleInsuranceInfo(state = Funding).toData, 8, Data.Constr(5, PList.Nil))
        assertFailsWith("PT1")(
          evalRaw(unknownState, expire, rawCtx(unknownState, unknownState, beginningOfTime))
        )
    }

    test(
      "expiry is rejected for Initiated policies and on-risk policies without start date (ERR-MAIN-1)"
    ) {
        val initiated = sampleInsuranceInfo(state = Initiated)
        assertFailsWith("ERR-MAIN-1")(
          run(
            initiated,
            PolicyExpire,
            expireCtx(initiated, initiated.toData, beginningOfTime + days(999))
          )
        )
        val noStart = sampleInsuranceInfo(state = OnRisk)
        assertFailsWith("ERR-MAIN-1")(
          run(
            noStart,
            PolicyExpire,
            expireCtx(noStart, noStart.toData, beginningOfTime + days(999))
          )
        )
    }
}
