package fida.contract.toolbox

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v1.PosixTime

/** Test DSL mirroring the Haskell toolbox: `good` scenarios must run without errors, `bad`
  * scenarios must be rejected on chain with the expected error code in the script trace. Each
  * scenario runs in a fresh emulator session.
  */
trait TestToolbox extends AnyFunSuite {

    def good(name: String)(scenario: Run ?=> Any): Unit =
        test(name) {
            given Run = new Run
            scenario
        }

    def bad(name: String, expectedError: String)(scenario: Run ?=> Any): Unit =
        test(name) {
            given Run = new Run
            val failure = intercept[TxFailure](scenario)
            assert(
              failure.logs.contains(expectedError),
              s"expected $expectedError in the script trace, got ${failure.logs.mkString("[", ", ", "]")}: ${failure.message}"
            )
        }

    def setupUsers(using run: Run): Users = run.users

    def currentTime(using run: Run): PosixTime = run.currentTime

    def waitNSlots(n: Long)(using run: Run): Unit = run.waitNSlots(n)

    def waitUntil(time: PosixTime)(using run: Run): Unit = run.waitUntil(time)

    def days(n: Int): BigInt = Time.days(n)

    def seconds(n: Int): BigInt = Time.seconds(n)

    /** `n` ADA in lovelace. */
    def ada(n: Long): BigInt = BigInt(n) * 1_000_000
}

/** Port of `Fida.Contract.TestToolbox.Time`. */
object Time {
    def days(n: Int): BigInt = BigInt(1000L * 60 * 60 * 24) * n
    def seconds(n: Int): BigInt = BigInt(1000) * n
}
