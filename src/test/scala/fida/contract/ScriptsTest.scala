package fida.contract

import org.scalatest.funsuite.AnyFunSuite

/** Every contract lowers to a Plutus V2, UPLC 1.0.0 program. */
class ScriptsTest extends AnyFunSuite {
    for (name, compiled) <- Scripts.serialisable do
        test(s"$name compiles to a UPLC 1.0.0 Plutus V2 script") {
            val program = compiled.program
            assert(program.version == (1, 0, 0))
            assert(compiled.script.script.size > 0)
            info(s"$name: ${compiled.script.script.size} bytes")
        }
}
