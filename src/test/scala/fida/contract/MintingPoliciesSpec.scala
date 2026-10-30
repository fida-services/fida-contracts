package fida.contract

import fida.contract.testkit.{ScriptAssertions, ScriptEval}
import fida.contract.testkit.ContextBuilder.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v1.{
    Credential,
    DCert,
    PubKeyHash,
    ScriptPurpose,
    StakingCredential,
    TxOutRef
}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo, TxInfo}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.Result

/** Direct-evaluation tests of the InsuranceId and SystemId minting policies and the investor
  * validator, with their parameters applied as `Data` like off-chain code applies them.
  */
class MintingPoliciesSpec extends AnyFunSuite with ScriptAssertions {

    /** The currency symbol of the policy being run; the policies do not inspect it. */
    private val ownSymbol = ByteString.fromHex("cc" * 28)

    private def input(ref: TxOutRef): TxInInfo =
        txIn(ref, txOutNoDatum(pubKeyAddress(pkh(9)), value(5_000_000)))

    /** Every non-minting purpose, for a transaction spending `ref`. */
    private def nonMintingPurposes(ref: TxOutRef): Seq[ScriptPurpose] = Seq(
      ScriptPurpose.Spending(ref),
      ScriptPurpose.Rewarding(StakingCredential.StakingHash(Credential.PubKeyCredential(pkh(9)))),
      ScriptPurpose.Certifying(DCert.Mir)
    )

    // InsuranceId -----------------------------------------------------------------------------

    private val seedRef = outRef(7, 2)

    private def runInsuranceId(ctx: ScriptContext): Result =
        ScriptEval.eval(
          Scripts.insuranceIdMintingPolicy.program,
          seedRef.toData,
          Data.unit,
          ctx.toData
        )

    test("InsuranceId policy: mints when the parameter TxOutRef is spent") {
        assertSucceeds(runInsuranceId(minting(txInfo(inputs = Seq(input(seedRef))), ownSymbol)))
        val among = txInfo(inputs = Seq(input(outRef(1)), input(seedRef), input(outRef(8, 2))))
        assertSucceeds(runInsuranceId(minting(among, ownSymbol)))
    }

    test("InsuranceId policy: ERR-IID-0 when the parameter TxOutRef is not spent") {
        val notSpent = Seq(
          txInfo(),
          txInfo(inputs = Seq(input(outRef(1)), input(outRef(8, 2)))),
          // same transaction, other output; same output index, other transaction
          txInfo(inputs = Seq(input(outRef(7, 0)), input(outRef(7, 1)), input(outRef(6, 2)))),
          // referencing the output is not spending it
          txInfo(inputs = Seq(input(outRef(1))), referenceInputs = Seq(input(seedRef)))
        )
        for info <- notSpent do
            assertFailsWith("ERR-IID-0")(runInsuranceId(minting(info, ownSymbol)))
    }

    test("InsuranceId policy: ERR-IID-1 when not run as a minting policy") {
        val info = txInfo(inputs = Seq(input(seedRef)))
        for purpose <- nonMintingPurposes(seedRef) do
            assertFailsWith("ERR-IID-1")(runInsuranceId(ScriptContext(info, purpose)))
    }

    // SystemId --------------------------------------------------------------------------------

    private val governance = pkh(10)
    private val magic = BigInt(42)

    private def runSystemId(ctx: ScriptContext, magic: BigInt = magic): Result =
        ScriptEval.eval(
          Scripts.systemIdMintingPolicy.program,
          governance.toData,
          magic.toData,
          Data.unit,
          ctx.toData
        )

    private def signedBy(signatories: PubKeyHash*): TxInfo =
        txInfo(inputs = Seq(input(outRef(1))), signatories = signatories)

    test("SystemId policy: mints when signed by the system governance key") {
        assertSucceeds(runSystemId(minting(signedBy(governance), ownSymbol)))
        assertSucceeds(runSystemId(minting(signedBy(pkh(3), governance, pkh(4)), ownSymbol)))
    }

    test("SystemId policy: ERR-SID-0 when not signed by the system governance key") {
        assertFailsWith("ERR-SID-0")(runSystemId(minting(signedBy(), ownSymbol)))
        assertFailsWith("ERR-SID-0")(runSystemId(minting(signedBy(pkh(3), pkh(4)), ownSymbol)))
    }

    test("SystemId policy: ERR-SID-1 when not run as a minting policy") {
        for purpose <- nonMintingPurposes(outRef(1)) do
            assertFailsWith("ERR-SID-1")(runSystemId(ScriptContext(signedBy(governance), purpose)))
    }

    test("SystemId policy: the magic number only tells deployments apart") {
        val ctx = minting(signedBy(governance), ownSymbol)
        for magic <- Seq(BigInt(0), BigInt(1), BigInt(-7), BigInt(2).pow(70)) do
            assertSucceeds(runSystemId(ctx, magic))
        assert(
          Scripts.systemIdMintingPolicyScript(governance, 0).scriptHash !=
              Scripts.systemIdMintingPolicyScript(governance, 1).scriptHash
        )
    }

    // Investor --------------------------------------------------------------------------------

    test("Investor validator accepts any datum and redeemer") {
        val investorRef = outRef(3)
        val ctx = spending(txInfo(inputs = Seq(input(investorRef))), investorRef)
        val datums = Seq(Data.unit, Data.I(42), Data.B(ByteString.fromHex("00ff")))
        val redeemers = Seq(
          InvestorRedeemer.ProcessClaim.toData,
          InvestorRedeemer.Withdraw.toData,
          Data.unit,
          Data.Constr(9, scalus.cardano.onchain.plutus.prelude.List(Data.I(1))),
          Data.List(scalus.cardano.onchain.plutus.prelude.List.Nil)
        )
        for datum <- datums; redeemer <- redeemers do
            assertSucceeds(
              ScriptEval.eval(
                Scripts.investorValidator.program,
                pkh(5).toData,
                SystemId(ownSymbol).toData,
                datum,
                redeemer,
                ctx.toData
              )
            )
    }
}
