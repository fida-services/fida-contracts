package fida.serialise

import fida.contract.Scripts
import scalus.cardano.ledger.Language
import scalus.uplc.CompiledPlutus

import java.io.PrintStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec

/** Serialises the Fida contracts for off-chain code (port of the Haskell `fida-contracts-serialise`
  * executable, `app/serialise/Main.hs`).
  *
  * {{{
  * Usage: fida-contracts-serialise (-o|--output-script-dir DIR |
  *                                  -n|--purs-module-name MODULE [-p|--output-purs-file PATH])
  * }}}
  *
  *   - `-o DIR` writes every contract of [[Scripts.serialisable]] to `DIR/<Name>.script` as a
  *     cardano-api text envelope, laid out like cardano-api's `writeFileTextEnvelope` output;
  *   - `-n MODULE` emits a PureScript module (for cardano-transaction-lib) that exports each
  *     contract's text envelope as `raw<Name> :: String`, to `PATH` with `-p`, else to standard
  *     output.
  *
  * The contracts are serialised unapplied, as in the Haskell tool: off-chain code applies their
  * parameters itself (CTL `applyArgs`).
  */
object Main {

    /** A parsed command line. */
    enum Command:
        case GenerateScripts(outputDir: String)
        case GeneratePursModule(moduleName: String, outputFile: Option[String])
        case ShowHelp

    val programName = "fida-contracts-serialise"

    val usage: String =
        s"""Usage: $programName (-o|--output-script-dir DIR |
           |                                -n|--purs-module-name MODULE
           |                                [-p|--output-purs-file PATH])
           |
           |  Outputs plutus scripts
           |
           |Available options:
           |  -o,--output-script-dir DIR
           |                           Write each script to DIR/<Name>.script as a
           |                           cardano-api text envelope
           |  -n,--purs-module-name MODULE
           |                           Module name example: Fida.RawScripts
           |  -p,--output-purs-file PATH
           |                           Write the PureScript module to PATH instead of
           |                           standard output
           |  -h,--help                Show this help text""".stripMargin

    def main(args: Array[String]): Unit =
        val status = run(args.toSeq, System.out, System.err)
        if status != 0 then sys.exit(status)

    /** Runs the tool on `args`, writing messages to `out` and `err`; returns the exit status. */
    def run(args: Seq[String], out: PrintStream, err: PrintStream): Int =
        parseArgs(args) match
            case Left(error) =>
                err.println(error)
                err.println()
                err.println(usage)
                1
            case Right(Command.ShowHelp) =>
                out.println(usage)
                0
            case Right(Command.GenerateScripts(outputDir)) =>
                writeScripts(outputDir, Scripts.serialisable, out)
                0
            case Right(Command.GeneratePursModule(moduleName, outputFile)) =>
                val module = pursModule(moduleName, Scripts.serialisable)
                outputFile match
                    case Some(path) => writeFile(Paths.get(path), module)
                    case None       => out.print(module)
                out.flush()
                0

    // Command line ---------------------------------------------------------------------------

    /** Option keys, by every spelling optparse-applicative accepted in the Haskell tool. */
    private val optionKeys: Map[String, Char] = Map(
      "-o" -> 'o',
      "--output-script-dir" -> 'o',
      "-n" -> 'n',
      "--purs-module-name" -> 'n',
      "-p" -> 'p',
      "--output-purs-file" -> 'p'
    )

    private val helpFlags = Set("-h", "--help")

    /** Parses the command line. Options take their value from the next argument, or inline as
      * `--name=VALUE` / `-xVALUE`.
      */
    def parseArgs(args: Seq[String]): Either[String, Command] =
        @tailrec
        def loop(rest: List[String], acc: Map[Char, String]): Either[String, Map[Char, String]] =
            rest match
                case Nil                                   => Right(acc)
                case flag :: _ if helpFlags.contains(flag) => Right(Map('h' -> ""))
                case arg :: tail                           =>
                    val (name, inlineValue) =
                        if arg.startsWith("--") then
                            arg.indexOf('=') match
                                case -1 => (arg, None)
                                case i  => (arg.take(i), Some(arg.drop(i + 1)))
                        else if arg.startsWith("-") && arg.length > 2 then
                            (arg.take(2), Some(arg.drop(2)))
                        else (arg, None)
                    optionKeys.get(name) match
                        case None if arg.startsWith("-")    => Left(s"Invalid option `$arg'")
                        case None                           => Left(s"Invalid argument `$arg'")
                        case Some(key) if acc.contains(key) =>
                            Left(s"Option `$name' given more than once")
                        case Some(key) =>
                            (inlineValue, tail) match
                                case (Some(value), _) => loop(tail, acc.updated(key, value))
                                case (None, value :: afterValue) =>
                                    loop(afterValue, acc.updated(key, value))
                                case (None, Nil) => Left(s"The option `$name' expects an argument.")

        loop(args.toList, Map.empty).flatMap { opts =>
            if opts.contains('h') then Right(Command.ShowHelp)
            else
                (opts.get('o'), opts.get('n'), opts.get('p')) match
                    case (Some(dir), None, None) => Right(Command.GenerateScripts(dir))
                    case (Some(_), _, _)         =>
                        Left("Option `-o' cannot be combined with `-n' or `-p'")
                    case (None, Some(module), file) =>
                        Right(Command.GeneratePursModule(module, file))
                    case (None, None, Some(_)) => Left("Missing: -n|--purs-module-name MODULE")
                    case (None, None, None)    =>
                        Left("Missing: (-o|--output-script-dir DIR | -n|--purs-module-name MODULE)")
        }

    // Output ---------------------------------------------------------------------------------

    /** Writes each script to `outputDir/<Name>.script`, reporting every file on `out`. */
    def writeScripts(
        outputDir: String,
        scripts: Seq[(String, CompiledPlutus[?])],
        out: PrintStream
    ): Unit =
        Files.createDirectories(Paths.get(outputDir))
        for (name, compiled) <- scripts do
            val file = s"$outputDir/$name.script"
            val envelope = textEnvelope(compiled)
            out.println(s"Serializing $file")
            writeFile(Paths.get(file), envelope)

    /** The PureScript module exporting each script's compact text envelope as
      * `raw<Name> :: String`, line for line as the Haskell tool printed it.
      */
    def pursModule(moduleName: String, scripts: Seq[(String, CompiledPlutus[?])]): String =
        val named = scripts.map((name, compiled) => (s"raw$name", compiled))
        val lines = Seq.newBuilder[String]
        lines += "-- This file is autogenerated."
        lines += s"module $moduleName"
        lines += " ("
        named.map(_._1) match
            case first +: rest =>
                lines += s"   $first"
                lines ++= rest.map(name => s" , $name")
            case _ => ()
        lines += " ) where"
        for (name, compiled) <- named do
            lines += ""
            lines += s"$name :: String"
            lines += s"$name ="
            lines += "   \"\"\"" + compactTextEnvelope(compiled) + "\"\"\""
        lines += ""
        lines.result().map(_ + "\n").mkString

    /** The script as a cardano-api text envelope file: pretty-printed JSON with a 4-space indent,
      * keys in the order `type`, `description`, `cborHex`, and a trailing newline (cardano-api's
      * `textEnvelopeToJSON`).
      */
    def textEnvelope(compiled: CompiledPlutus[?]): String =
        envelopeFields(compiled)
            .map((key, value) => s"    ${jsonString(key)}: ${jsonString(value)}")
            .mkString("{\n", ",\n", "\n}\n")

    /** The script as a compact text envelope JSON object with alphabetically sorted keys, as
      * aeson's `encode` renders cardano-api's `TextEnvelope`.
      */
    def compactTextEnvelope(compiled: CompiledPlutus[?]): String =
        envelopeFields(compiled)
            .sortBy(_._1)
            .map((key, value) => s"${jsonString(key)}:${jsonString(value)}")
            .mkString("{", ",", "}")

    /** Text envelope fields in cardano-api order. `cborHex` is the double-CBOR-encoded flat program
      * (cardano-api's `serialiseToCBOR` of a `PlutusScriptSerialised`).
      */
    private def envelopeFields(compiled: CompiledPlutus[?]): Seq[(String, String)] = Seq(
      "type" -> envelopeType(compiled.language),
      "description" -> "",
      "cborHex" -> compiled.program.doubleCborHex
    )

    /** cardano-api's text envelope type of a Plutus script. */
    def envelopeType(language: Language): String = language match
        case Language.PlutusV1 => "PlutusScriptV1"
        case Language.PlutusV2 => "PlutusScriptV2"
        case Language.PlutusV3 => "PlutusScriptV3"
        case other => throw IllegalArgumentException(s"no text envelope type for $other scripts")

    private[serialise] def jsonString(s: String): String =
        val sb = StringBuilder("\"")
        s.foreach {
            case '"'          => sb.append("\\\"")
            case '\\'         => sb.append("\\\\")
            case '\n'         => sb.append("\\n")
            case '\r'         => sb.append("\\r")
            case '\t'         => sb.append("\\t")
            case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
            case c            => sb.append(c)
        }
        sb.append('"').toString

    private def writeFile(path: Path, content: String): Unit =
        Option(path.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
        Files.writeString(path, content, UTF_8)
}
