package fida.contract.testkit

import fida.contract.{InvestorValidator, Scripts, SystemIdMintingPolicy}
import fida.contract.insurance.{
    InsuranceIdMintingPolicy,
    InsurancePolicyValidator,
    PiggyBankValidator
}
import org.scalatest.{Assertions, Canceled, TestSuite, TestSuiteMixin}
import org.scalatest.exceptions.TestFailedException
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.OnchainError
import scalus.uplc.Program
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.cardano.onchain.plutus.v1.{
    Address,
    Credential,
    Interval,
    PolicyId,
    PubKeyHash,
    ScriptPurpose,
    TokenName,
    TxId,
    TxOutRef,
    ValidatorHash,
    Value
}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInInfo, TxInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.{AssocMap, List, Option, SortedMap}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import scala.util.{Failure, Success, Try}

/** Whether the contracts are compiled to UPLC in this build. */
object Uplc {

    /** False in the `jvm` build, which compiles the contracts without the Scalus plugin so that
      * scoverage and stryker4s can work on them (see build.sbt).
      */
    val available: Boolean = !sys.props.get("fida.uplc").contains("false")
}

/** For suites that need compiled scripts (the emulator, the serialise tool): their tests are
  * canceled in the `jvm` build.
  */
trait RequiresUplc extends TestSuiteMixin { this: TestSuite =>

    abstract override def withFixture(test: NoArgTest): org.scalatest.Outcome =
        if Uplc.available then super.withFixture(test)
        else Canceled("needs the compiled scripts, which the jvm build does not have")
}

/** The result of running a script: success or failure, and the trace messages. */
final case class Outcome(isSuccess: Boolean, logs: Seq[String]) {
    def isFailure: Boolean = !isSuccess
}

/** A script under test: its compiled program, and its `untyped` entry point to run the same code on
  * the JVM. Both take the parameters first, then the datum (validators), redeemer and context.
  */
final case class OnChainScript(
    name: String,
    arity: Int,
    program: () => Program,
    jvm: Seq[Data] => Unit
)

object OnChainScript {

    val insuranceIdMintingPolicy: OnChainScript = OnChainScript(
      "InsuranceIdMintingPolicy",
      3,
      () => Scripts.insuranceIdMintingPolicy.program,
      args => InsuranceIdMintingPolicy.untyped(args(0), args(1), args(2))
    )

    val insurancePolicyValidator: OnChainScript = OnChainScript(
      "InsurancePolicyValidator",
      4,
      () => Scripts.insurancePolicyValidator.program,
      args => InsurancePolicyValidator.untyped(args(0), args(1), args(2), args(3))
    )

    val piggyBankValidator: OnChainScript = OnChainScript(
      "PiggyBankValidator",
      5,
      () => Scripts.piggyBankValidator.program,
      args => PiggyBankValidator.untyped(args(0), args(1), args(2), args(3), args(4))
    )

    val investorValidator: OnChainScript = OnChainScript(
      "InvestorValidator",
      5,
      () => Scripts.investorValidator.program,
      args => InvestorValidator.untyped(args(0), args(1), args(2), args(3), args(4))
    )

    val systemIdMintingPolicy: OnChainScript = OnChainScript(
      "SystemIdMintingPolicy",
      4,
      () => Scripts.systemIdMintingPolicy.program,
      args => SystemIdMintingPolicy.untyped(args(0), args(1), args(2), args(3))
    )
}

/** Direct (emulator-free) evaluation of scripts against hand-built V2 script contexts.
  *
  * Use it to unit-test validator branches precisely: build a [[ScriptContext]] with
  * [[ContextBuilder]], apply parameters, datum, redeemer and context, and check the result and its
  * trace logs.
  */
object ScriptEval {

    /** A Plutus V2 machine at the protocol version the emulator (Cardano mainnet) runs. */
    given vm: PlutusVM = PlutusVM.makePlutusV2VM(MajorProtocolVersion.vanRossemPV)

    /** Evaluates `program` applied to `args` as UPLC. */
    def eval(program: Program, args: Data*): Result =
        args.foldLeft(program)(_ $ _).deBruijnedProgram.evaluateDebug

    /** Runs `script` on `args` on the JVM and, when the build compiles the contracts, as UPLC,
      * failing the test unless both runs have the same outcome and trace.
      */
    def eval(script: OnChainScript, args: Data*): Outcome =
        require(args.size == script.arity, s"${script.name} takes ${script.arity} arguments")
        val onJvm = JvmEval.run(script.jvm(args))
        if !Uplc.available then onJvm
        else
            val result = eval(script.program(), args*)
            val onUplc = Outcome(result.isSuccess, result.logs)
            if onUplc != onJvm then
                throw new TestFailedException(
                  s"${script.name} ran differently as UPLC ($onUplc) and on the JVM ($onJvm)",
                  0
                )
            onUplc
}

/** Runs on-chain code as Scala on the JVM, where traces are printed and errors are thrown. */
object JvmEval {

    /** Runs `code`, collecting its traces; an `OnchainError` (`fail(message)`) adds its message to
      * the trace, as the UPLC error traces do.
      */
    def run(code: => Unit): Outcome =
        val out = ByteArrayOutputStream()
        val result = Try(Console.withOut(PrintStream(out, true, UTF_8))(code))
        val traces = out.toString(UTF_8).linesIterator.toSeq
        result match
            case Success(_)               => Outcome(true, traces)
            case Failure(e: OnchainError) => Outcome(false, traces :+ e.getMessage)
            case Failure(_)               => Outcome(false, traces)
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

    def assertSucceeds(result: Outcome): Unit =
        assert(result.isSuccess, s"expected success, got failure with logs ${result.logs}")

    /** Asserts failure and that `code` (e.g. `ERR-INIT-1`) is among the trace logs. */
    def assertFailsWith(code: String)(result: Outcome): Unit =
        assert(result.isFailure, s"expected failure with $code, but the script succeeded")
        assert(
          result.logs.contains(code),
          s"expected $code in the trace logs, got ${result.logs.mkString("[", ", ", "]")}"
        )
}
