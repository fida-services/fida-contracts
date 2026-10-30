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

    private def run(datum: InsurancePolicyDatum, redeemer: InsurancePolicyRedeemer, ctx: ScriptContext): Result =
        ScriptEval.eval(
          Scripts.insurancePolicyValidator.program,
          insuranceId.toData,
          datum.toData,
          redeemer.toData,
          ctx.toData
        )

    /** Spends the policy info UTxO holding `datum` and recreates it with `outDatum`. */
    private def expireCtx(datum: InsurancePolicyDatum, outDatum: Data, validFrom: PosixTime): ScriptContext =
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
        assertSucceeds(run(datum, PolicyExpire, expireCtx(datum, expired.toData, datum.iInfoFundingDeadline + 1)))
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
        assertSucceeds(run(datum, PolicyExpire, expireCtx(datum, expired.toData, start + days(365) + 1)))
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

    /** `datum` with its 15th field (`iInfoClaim`) replaced by `claim`. */
    private def withClaimField(datum: Data, claim: Data): Data = datum match
        case Data.Constr(tag, fields) => Data.Constr(tag, PList.from(fields.toScalaList.updated(14, claim)))
        case other                    => fail(s"not a constructor: $other")

    // Characterises lazy decoding on malformed data (docs/MIGRATION.md): fields the validator does
    // not change are carried over as received. PlutusTx decoded and re-encoded the whole datum, so
    // it expected the canonical `Nothing` (`Constr 1 []`) in the output instead.
    test("unchanged fields keep their received encoding in the expected output datum") {
        val nonCanonicalNothing = Data.Constr(1, PList.from(Seq(Data.I(0))))
        val datum = sampleInsuranceInfo(state = Funding)
        val input = withClaimField(datum.toData, nonCanonicalNothing)
        val expired = sampleInsuranceInfo(state = Expired).toData
        def ctxWithOutput(out: Data): ScriptContext =
            val in = txIn(policyRef, txOut(policyAddress, value(2_000_000, infoNft), input))
            spending(
              txInfo(
                inputs = Seq(in),
                outputs = Seq(txOut(policyAddress, value(2_000_000, infoNft), out)),
                validRange = Interval.after(datum.iInfoFundingDeadline + 1)
              ),
              policyRef
            )
        def runRaw(out: Data): Result = ScriptEval.eval(
          Scripts.insurancePolicyValidator.program,
          insuranceId.toData,
          input,
          (PolicyExpire: InsurancePolicyRedeemer).toData,
          ctxWithOutput(out).toData
        )
        assertSucceeds(runRaw(withClaimField(expired, nonCanonicalNothing)))
        assertFailsWith("ERR-MAIN-3")(runRaw(expired))
    }

    test("expiry is rejected for Initiated policies and on-risk policies without start date (ERR-MAIN-1)") {
        val initiated = sampleInsuranceInfo(state = Initiated)
        assertFailsWith("ERR-MAIN-1")(
          run(initiated, PolicyExpire, expireCtx(initiated, initiated.toData, beginningOfTime + days(999)))
        )
        val noStart = sampleInsuranceInfo(state = OnRisk)
        assertFailsWith("ERR-MAIN-1")(
          run(noStart, PolicyExpire, expireCtx(noStart, noStart.toData, beginningOfTime + days(999)))
        )
    }
}
