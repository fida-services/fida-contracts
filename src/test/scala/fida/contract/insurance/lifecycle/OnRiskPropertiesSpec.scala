package fida.contract.insurance.lifecycle

import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PolicyClaimPayment}
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyOnRisk
import fida.contract.insurance.InsurancePolicyState.OnRisk
import fida.contract.insurance.PolicyOnRiskRedeemer.PolicyOnRiskClaimPayment
import fida.contract.insurance.Tokens
import fida.contract.testkit.{OnChainScript, Outcome, ScriptAssertions, ScriptEval, Shape}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import fida.contract.testkit.Shape.Corruption
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

/** The claim payment action recognises policy info like Haskell's `fromBuiltinData`: only an exact
  * `InsuranceInfo` datum counts.
  */
class OnRiskPropertiesSpec extends AnyFunSuite with ScriptAssertions {

    private val policyAddress = scriptAddress(scriptHash(1))
    private val infoNft = (insuranceId.cs, Tokens.policyInfoTokenName, BigInt(1))
    private val paymentRef = outRef(7)

    /** The policy holder of the [[Shape.covering]] policy info, who signs the claim payment. */
    private val holder = PubKeyHash(Shape.sampleBytes)

    private val signed: InsuranceInfo =
        sampleInsuranceInfo(state = OnRisk).copy(iInfoPolicyHolder = holder) match
            case info: InsuranceInfo => info
            case other               => fail(s"not an InsuranceInfo: $other")

    /** Spends a claim payment, signed by [[holder]], with reference inputs holding the policy info
      * NFT and `policyInfos`.
      */
    private def payClaim(policyInfos: Data*): Outcome =
        val in =
            txIn(paymentRef, txOut(policyAddress, value(10_000_000), PolicyClaimPayment.toData))
        val refs = policyInfos.zipWithIndex.map { (d, i) =>
            txIn(outRef(20 + i), txOut(policyAddress, value(2_000_000, infoNft), d))
        }
        val ctx = spending(
          txInfo(inputs = Seq(in), referenceInputs = refs, signatories = Seq(holder)),
          paymentRef
        )
        ScriptEval.eval(
          OnChainScript.insurancePolicyValidator,
          insuranceId.toData,
          PolicyClaimPayment.toData,
          PolicyOnRisk(PolicyOnRiskClaimPayment).toData,
          ctx.toData
        )

    test("ClaimPayment counts policy info only with an exact InsuranceInfo datum") {
        assertSucceeds(payClaim(signed.toData))
        for d <- Shape.covering(Shape.insuranceInfo) do
            withClue(s"candidate $d")(assertFailsWith("ERR-ON-RISK-12")(payClaim(signed.toData, d)))
            for
                corruption <- Corruption.values.toSeq
                bad <- Shape.corruptions(Shape.insuranceInfo, d, corruption)
            do withClue(s"candidate $bad")(assertSucceeds(payClaim(signed.toData, bad)))
    }
}
