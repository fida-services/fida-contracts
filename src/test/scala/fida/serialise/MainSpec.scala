package fida.serialise

import fida.contract.Scripts
import fida.contract.testkit.RequiresUplc
import fida.serialise.Main.Command
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.{CompiledPlutus, Program}
import scalus.utils.Utils.readPlutusFileContent

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.jdk.CollectionConverters.*

/** The serialise tool produces what the Haskell `fida-contracts-serialise` produced. */
class MainSpec extends AnyFunSuite with RequiresUplc {

    /** The serialisable scripts, in the order and under the names of the Haskell tool. */
    private lazy val expectedScripts: Seq[(String, CompiledPlutus[?])] = Seq(
      "InsuranceIdMintingPolicy" -> Scripts.insuranceIdMintingPolicy,
      "InsurancePolicyValidator" -> Scripts.insurancePolicyValidator,
      "InvestorValidator" -> Scripts.investorValidator,
      "SystemIdMintingPolicy" -> Scripts.systemIdMintingPolicy,
      "PiggyBankValidator" -> Scripts.piggyBankValidator
    )

    /** cardano-api `writeFileTextEnvelope` layout of a Plutus V2 script. */
    private val textEnvelopeFile =
        "\\{\n    \"type\": \"PlutusScriptV2\",\n    \"description\": \"\",\n    \"cborHex\": \"([0-9a-f]+)\"\n\\}\n".r

    /** aeson `encode` of a Plutus V2 script's text envelope (keys sorted). */
    private val compactTextEnvelope =
        "\\{\"cborHex\":\"([0-9a-f]+)\",\"description\":\"\",\"type\":\"PlutusScriptV2\"\\}".r

    private val tripleQuote = "\"\"\""

    private case class Outcome(status: Int, out: String, err: String)

    private def runMain(args: String*): Outcome =
        val out = ByteArrayOutputStream()
        val err = ByteArrayOutputStream()
        val status = Main.run(args, PrintStream(out, true, UTF_8), PrintStream(err, true, UTF_8))
        Outcome(status, out.toString(UTF_8), err.toString(UTF_8))

    private def withTempDir[A](body: Path => A): A =
        val dir = Files.createTempDirectory("fida-serialise")
        try body(dir)
        finally Files.walk(dir).sorted(Comparator.reverseOrder()).forEach(Files.delete(_))

    /** `program` is `compiled`'s program (alpha-equivalence needs de Bruijn indices). */
    private def assertIsProgramOf(
        name: String,
        program: Program,
        compiled: CompiledPlutus[?]
    ): Unit =
        assert(program.version == (1, 0, 0), s"$name: UPLC version ${program.version}")
        assert(
          program.deBruijnedProgram alphaEq compiled.program.deBruijnedProgram,
          s"$name: decoded program differs"
        )

    /** Decodes a `cborHex` field and checks it is exactly the compiled program. */
    private def assertSameProgram(
        name: String,
        cborHex: String,
        compiled: CompiledPlutus[?]
    ): Unit =
        assert(cborHex == compiled.program.doubleCborHex, s"$name: unexpected cborHex")
        assertIsProgramOf(name, Program.fromDoubleCborHex(cborHex), compiled)

    test("serialises the five Haskell scripts, in order, under their Haskell names") {
        assert(Scripts.serialisable.map(_._1) == expectedScripts.map(_._1))
        for ((_, actual), (name, expected)) <- Scripts.serialisable.zip(expectedScripts) do
            assert(actual eq expected, name)
    }

    test("-o writes a cardano-api text envelope <Name>.script per script") {
        withTempDir { dir =>
            val outcome = runMain("-o", dir.toString)
            assert(outcome.status == 0, outcome.err)
            assert(
              outcome.out.linesIterator.toSeq ==
                  expectedScripts.map((name, _) => s"Serializing $dir/$name.script")
            )
            val files = Files.list(dir).iterator.asScala.map(_.getFileName.toString).toSeq
            assert(files.sorted == expectedScripts.map(_._1 + ".script").sorted)
            for (name, compiled) <- expectedScripts do
                val content = Files.readString(dir.resolve(s"$name.script"), UTF_8)
                content match
                    case textEnvelopeFile(cborHex) => assertSameProgram(name, cborHex, compiled)
                    case _ => fail(s"$name.script is not a text envelope:\n$content")
                // Scalus reads the files back as well (it parses the JSON)
                assertIsProgramOf(name, readPlutusFileContent(content), compiled)
        }
    }

    test("--output-script-dir=DIR creates the directory and overwrites existing files") {
        withTempDir { tmp =>
            val dir = tmp.resolve("nested/scripts")
            val stale = dir.resolve("InvestorValidator.script")
            Files.createDirectories(dir)
            Files.writeString(stale, "stale content that is longer than the new envelope " * 100)
            val outcome = runMain(s"--output-script-dir=$dir")
            assert(outcome.status == 0, outcome.err)
            assert(Files.list(dir).count() == 5)
            Files.readString(stale, UTF_8) match
                case textEnvelopeFile(cborHex) =>
                    assertSameProgram("InvestorValidator", cborHex, Scripts.investorValidator)
                case other => fail(s"not overwritten:\n$other")
        }
    }

    /** The PureScript module, line by line as the Haskell tool emitted it. */
    private def expectedPursModule(moduleName: String): String =
        val header = Seq(
          "-- This file is autogenerated.",
          s"module $moduleName",
          " (",
          "   rawInsuranceIdMintingPolicy",
          " , rawInsurancePolicyValidator",
          " , rawInvestorValidator",
          " , rawSystemIdMintingPolicy",
          " , rawPiggyBankValidator",
          " ) where"
        )
        val definitions = expectedScripts.flatMap { (name, compiled) =>
            val envelope = "{\"cborHex\":\"" + compiled.program.doubleCborHex +
                "\",\"description\":\"\",\"type\":\"PlutusScriptV2\"}"
            Seq("", s"raw$name :: String", s"raw$name =", s"   $tripleQuote$envelope$tripleQuote")
        }
        (header ++ definitions :+ "").map(_ + "\n").mkString

    test("-n MODULE -p PATH writes the PureScript module in the Haskell layout") {
        withTempDir { dir =>
            val file = dir.resolve("src/Fida/RawScripts.purs")
            val outcome = runMain("-n", "Fida.RawScripts", "-p", file.toString)
            assert(outcome.status == 0, outcome.err)
            assert(outcome.out.isEmpty)
            val module = Files.readString(file, UTF_8)
            assert(module == expectedPursModule("Fida.RawScripts"))
            assert(module.endsWith(s"$tripleQuote\n\n"))
            // every embedded envelope decodes to its program
            val envelopes = module.linesIterator
                .filter(_.startsWith(s"   $tripleQuote"))
                .map(_.trim.stripPrefix(tripleQuote).stripSuffix(tripleQuote))
                .toSeq
            assert(envelopes.size == expectedScripts.size)
            for (envelope, (name, compiled)) <- envelopes.zip(expectedScripts) do
                envelope match
                    case compactTextEnvelope(cborHex) => assertSameProgram(name, cborHex, compiled)
                    case _ => fail(s"$name: not a compact text envelope: $envelope")
        }
    }

    test("-n MODULE without -p prints the module to standard output") {
        val outcome = runMain("--purs-module-name", "Fida.Contracts.Raw")
        assert(outcome.status == 0, outcome.err)
        assert(outcome.out == expectedPursModule("Fida.Contracts.Raw"))
    }

    test("-p overwrites an existing file") {
        withTempDir { dir =>
            val file = dir.resolve("RawScripts.purs")
            Files.writeString(file, "x" * 1_000_000)
            assert(runMain("-nFida.RawScripts", s"--output-purs-file=$file").status == 0)
            assert(Files.readString(file, UTF_8) == expectedPursModule("Fida.RawScripts"))
        }
    }

    test("accepts every option spelling of the Haskell tool") {
        import Command.*
        val cases = Seq(
          Seq("-o", "out") -> GenerateScripts("out"),
          Seq("-oout") -> GenerateScripts("out"),
          Seq("--output-script-dir", "out") -> GenerateScripts("out"),
          Seq("--output-script-dir=out") -> GenerateScripts("out"),
          Seq("-n", "M") -> GeneratePursModule("M", None),
          Seq("-nM", "-pf.purs") -> GeneratePursModule("M", Some("f.purs")),
          Seq("-p", "f.purs", "-n", "M") -> GeneratePursModule("M", Some("f.purs")),
          Seq("--purs-module-name", "M", "--output-purs-file", "f.purs") ->
              GeneratePursModule("M", Some("f.purs")),
          Seq("--purs-module-name=M", "--output-purs-file=f.purs") ->
              GeneratePursModule("M", Some("f.purs")),
          Seq("-h") -> ShowHelp,
          Seq("-o", "out", "--help") -> ShowHelp
        )
        for (args, expected) <- cases do assert(Main.parseArgs(args) == Right(expected), args)
    }

    test("rejects invalid command lines") {
        val cases = Seq(
          Seq() -> "Missing: (-o|--output-script-dir DIR | -n|--purs-module-name MODULE)",
          Seq("-p", "f.purs") -> "Missing: -n|--purs-module-name MODULE",
          Seq("-o", "out", "-n", "M") -> "cannot be combined",
          Seq("-o", "out", "-p", "f.purs") -> "cannot be combined",
          Seq("-o", "a", "-o", "b") -> "given more than once",
          Seq("-o") -> "The option `-o' expects an argument.",
          Seq("--purs-module-name") -> "The option `--purs-module-name' expects an argument.",
          Seq("-x") -> "Invalid option `-x'",
          Seq("--output-dir=out") -> "Invalid option `--output-dir=out'",
          Seq("out") -> "Invalid argument `out'"
        )
        for (args, message) <- cases do
            Main.parseArgs(args) match
                case Left(error) => assert(error.contains(message), args)
                case Right(cmd)  => fail(s"$args parsed as $cmd")
    }

    test("prints the usage on --help, and with an error and status 1 on bad arguments") {
        val help = runMain("--help")
        assert(help.status == 0)
        assert(help.out.startsWith("Usage: fida-contracts-serialise (-o|--output-script-dir DIR |"))
        val bad = runMain("--bogus")
        assert(bad.status == 1)
        assert(bad.out.isEmpty)
        assert(bad.err.startsWith("Invalid option `--bogus'"))
        assert(bad.err.contains("Usage: fida-contracts-serialise"))
    }
}
