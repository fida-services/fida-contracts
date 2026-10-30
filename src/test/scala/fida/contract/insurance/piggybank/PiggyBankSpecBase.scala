package fida.contract.insurance.piggybank

import fida.contract.insurance.{FidaCardId, PiggyBankDatum, PiggyBankRedeemer, Tokens}
import fida.contract.testkit.{OnChainScript, Outcome, ScriptAssertions, ScriptEval}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{
    Address,
    Interval,
    PolicyId,
    PubKeyHash,
    TokenName,
    TxOutRef,
    Value
}
import scalus.cardano.onchain.plutus.v2.{ScriptContext, TxInInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.List

/** Shared fixtures for the direct-evaluation specs of
  * [[fida.contract.insurance.PiggyBankValidator]].
  *
  * The piggy bank under test backs fida card `1` of the sample insurance: its UTxO sits at
  * [[piggyBankAddress]] and is spent through [[piggyBankRef]]. Policy info reference inputs hold
  * the insurance's `POLICY-INFO` NFT with an inline `InsuranceInfo` datum, as in the Haskell test
  * toolbox.
  */
trait PiggyBankSpecBase extends AnyFunSuite with ScriptAssertions {

    val cs: PolicyId = insuranceId.cs
    val fcid: FidaCardId = fidaCardId(1)
    val cardToken: TokenName = Tokens.fidaCardTokenName(fcid.id)
    val cardNft: (PolicyId, TokenName, BigInt) = (cs, cardToken, BigInt(1))
    val statusNft: (PolicyId, TokenName, BigInt) = (cs, Tokens.fidaCardStatusTokenName, BigInt(1))
    val infoNft: (PolicyId, TokenName, BigInt) = (cs, Tokens.policyInfoTokenName, BigInt(1))

    val minAda: BigInt = 2_000_000

    val piggyBankAddress: Address = scriptAddress(scriptHash(100))
    val piggyBankRef: TxOutRef = outRef(100)
    val policyAddress: Address = scriptAddress(scriptHash(1))
    val investorAddress: Address = pubKeyAddress(investor1)

    /** Evaluates the piggy bank validator (parameters: the sample insurance and `card`) on raw
      * datum and redeemer data.
      */
    def evalRaw(
        datum: Data,
        redeemer: Data,
        ctx: ScriptContext,
        card: Data = fcid.toData,
        insurance: Data = insuranceId.toData
    ): Outcome =
        ScriptEval.eval(
          OnChainScript.piggyBankValidator,
          insurance,
          card,
          datum,
          redeemer,
          ctx.toData
        )

    def eval(datum: PiggyBankDatum, redeemer: PiggyBankRedeemer, ctx: ScriptContext): Outcome =
        evalRaw(datum.toData, redeemer.toData, ctx)

    /** Fails, and `code` is the only `ERR-…` code in the logs, i.e. the error the script reports.
      */
    def assertRejectedWith(code: String)(result: Outcome): Unit =
        assertFailsWith(code)(result)
        val codes = result.logs.filter(_.startsWith("ERR-"))
        assert(
          codes == Seq(code),
          s"expected $code as the only error code, got ${codes.mkString("[", ", ", "]")}"
        )

    /** Fails without reporting any `ERR-…` code, with `marker` in the logs when given. */
    def assertFailsWithoutErrorCode(result: Outcome, marker: Option[String] = None): Unit =
        assert(
          result.isFailure,
          s"expected failure, but the script succeeded with logs ${result.logs}"
        )
        assert(
          !result.logs.exists(_.startsWith("ERR-")),
          s"unexpected error code in ${result.logs}"
        )
        marker.foreach(m =>
            assert(result.logs.contains(m), s"expected $m in the logs, got ${result.logs}")
        )

    def cardDatum(
        isSold: Boolean,
        cardValue: BigInt = fidaCardValue,
        paidClaims: Seq[ByteString] = Seq.empty
    ): PiggyBankDatum = PiggyBankDatum.PBankFidaCard(isSold, fcid, cardValue, List.from(paidClaims))

    /** The piggy bank UTxO being validated. */
    def piggyBankInput(datum: Data, v: Value): TxInInfo =
        txIn(piggyBankRef, txOut(piggyBankAddress, v, datum))

    /** An output paying back to the piggy bank (a continuing output). */
    def piggyBankOutput(datum: Data, v: Value): TxOut = txOut(piggyBankAddress, v, datum)

    /** A reference input holding the policy info NFT with `datum` inline. */
    def policyInfoRef(datum: Data, n: Int = 1): TxInInfo =
        txIn(outRef(n), txOut(policyAddress, value(minAda, infoNft), datum))

    /** A wallet input holding the fida card NFT: the transaction spends the card. */
    val cardOwnerInput: TxInInfo =
        txIn(outRef(200), txOutNoDatum(investorAddress, value(5_000_000, cardNft)))

    /** A wallet input holding plenty of ADA and no token. */
    val walletInput: TxInInfo =
        txIn(outRef(201), txOutNoDatum(investorAddress, value(BigInt(5_000_000_000L))))

    /** A transaction spending the piggy bank UTxO at [[piggyBankRef]]. */
    def spendPiggyBank(
        inputs: Seq[TxInInfo],
        refs: Seq[TxInInfo] = Seq.empty,
        outputs: Seq[TxOut] = Seq.empty,
        validRange: Interval = Interval.always,
        signatories: Seq[PubKeyHash] = Seq.empty
    ): ScriptContext =
        spending(
          txInfo(
            inputs = inputs,
            referenceInputs = refs,
            outputs = outputs,
            validRange = validRange,
            signatories = signatories
          ),
          piggyBankRef
        )

    // Raw data, to exercise decoding exactly as PlutusTx does

    def constrData(tag: Int, fields: Data*): Data = Data.Constr(BigInt(tag), List.from(fields))

    def listData(items: Data*): Data = Data.List(List.from(items))

    /** One value of each kind of data that is not a constructor. */
    val nonConstructorData: Seq[Data] = Seq(
      Data.I(0),
      Data.B(ByteString.fromString("datum")),
      Data.List(List(Data.I(1))),
      Data.Map(List((Data.I(1), Data.I(2))))
    )

    /** `d` (a constructor) with `extra` appended to its fields. */
    def withExtraField(d: Data, extra: Data = Data.I(0)): Data = d match
        case Data.Constr(tag, fields) => Data.Constr(tag, List.from(fields.toScalaList :+ extra))
        case other                    => fail(s"not a constructor: $other")

    /** `d` (a constructor) with its field at `index` replaced by `field`. */
    def withField(d: Data, index: Int, field: Data): Data = d match
        case Data.Constr(tag, fields) =>
            Data.Constr(tag, List.from(fields.toScalaList.updated(index, field)))
        case other => fail(s"not a constructor: $other")

    /** `d` (a constructor) with tag `tag`, same fields. */
    def withTag(d: Data, tag: Int): Data = d match
        case Data.Constr(_, fields) => Data.Constr(BigInt(tag), fields)
        case other                  => fail(s"not a constructor: $other")
}
