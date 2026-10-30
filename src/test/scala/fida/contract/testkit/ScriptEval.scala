package fida.contract.testkit

import org.scalatest.Assertions
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.uplc.Program
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Interval, PolicyId, PubKeyHash, ScriptPurpose, TokenName, TxId, TxOutRef, ValidatorHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInInfo, TxInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.{AssocMap, List, Option, SortedMap}

/** Direct (emulator-free) evaluation of compiled scripts against hand-built V2 script contexts.
  *
  * Use it to unit-test validator branches precisely: build a [[ScriptContext]] with
  * [[ContextBuilder]], apply parameters, datum, redeemer and context, and check the result and its
  * trace logs.
  */
object ScriptEval {

    /** A Plutus V2 machine at the protocol version the emulator (Cardano mainnet) runs. */
    given vm: PlutusVM = PlutusVM.makePlutusV2VM(MajorProtocolVersion.vanRossemPV)

    /** Evaluates `program` applied to `args` (parameters first, then datum/redeemer/context). */
    def eval(program: Program, args: Data*): Result =
        args.foldLeft(program)(_ $ _).deBruijnedProgram.evaluateDebug
}

/** Builders for V2 script contexts with empty defaults. */
object ContextBuilder {

    val dummyTxId: TxId = TxId(ByteString.fromHex("00" * 32))

    def txId(n: Int): TxId = TxId(ByteString.fromHex(f"$n%064x"))

    def outRef(n: Int, idx: Int = 0): TxOutRef = TxOutRef(txId(n), BigInt(idx))

    def pkh(n: Int): PubKeyHash = PubKeyHash(ByteString.fromHex(f"$n%056x"))

    def scriptHash(n: Int): ValidatorHash = ByteString.fromHex(f"${n + 0x5c000000}%056x")

    def pubKeyAddress(pkh: PubKeyHash): Address =
        Address(Credential.PubKeyCredential(pkh), Option.None)

    def scriptAddress(hash: ValidatorHash): Address =
        Address(Credential.ScriptCredential(hash), Option.None)

    /** A value holding `lovelace` plus the given `(policy, token, quantity)` assets. */
    def value(lovelace: BigInt, assets: (PolicyId, TokenName, BigInt)*): Value =
        assets.foldLeft(Value.lovelace(lovelace)) { case (acc, (cs, tn, q)) =>
            acc + Value(cs, tn, q)
        }

    def txOut(address: Address, value: Value, datum: Data): TxOut =
        TxOut(address, value, OutputDatum.OutputDatum(datum))

    def txOutNoDatum(address: Address, value: Value): TxOut = TxOut(address, value)

    def txIn(ref: TxOutRef, out: TxOut): TxInInfo = TxInInfo(ref, out)

    def txInfo(
        inputs: Seq[TxInInfo] = Seq.empty,
        referenceInputs: Seq[TxInInfo] = Seq.empty,
        outputs: Seq[TxOut] = Seq.empty,
        mint: Value = Value.zero,
        validRange: Interval = Interval.always,
        signatories: Seq[PubKeyHash] = Seq.empty,
        id: TxId = dummyTxId
    ): TxInfo = TxInfo(
      inputs = List.from(inputs),
      referenceInputs = List.from(referenceInputs),
      outputs = List.from(outputs),
      fee = Value.zero,
      mint = mint,
      dcert = List.Nil,
      withdrawals = SortedMap.empty,
      validRange = validRange,
      signatories = List.from(signatories),
      redeemers = AssocMap.empty,
      data = SortedMap.empty,
      id = id
    )

    def spending(txInfo: TxInfo, ref: TxOutRef): ScriptContext =
        ScriptContext(txInfo, ScriptPurpose.Spending(ref))

    def minting(txInfo: TxInfo, cs: PolicyId): ScriptContext =
        ScriptContext(txInfo, ScriptPurpose.Minting(cs))
}

/** Assertions on evaluation results. */
trait ScriptAssertions extends Assertions {

    def assertSucceeds(result: Result): Unit =
        assert(result.isSuccess, s"expected success, got $result with logs ${result.logs}")

    /** Asserts failure and that `code` (e.g. `ERR-INIT-1`) is among the trace logs. */
    def assertFailsWith(code: String)(result: Result): Unit =
        assert(result.isFailure, s"expected failure with $code, but the script succeeded")
        assert(
          result.logs.contains(code),
          s"expected $code in the trace logs, got ${result.logs.mkString("[", ", ", "]")}"
        )
}
