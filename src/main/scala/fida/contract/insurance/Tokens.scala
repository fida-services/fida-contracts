package fida.contract.insurance

import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.cardano.onchain.plutus.v1.TokenName

/** Token names used under an insurance's currency symbol (port of
  * `Fida.Contract.Insurance.Tokens`).
  */
@Compile
object Tokens {
    val policyInfoTokenName: TokenName = ByteString.fromString("POLICY-INFO")

    val policyPaymentTokenName: TokenName = ByteString.fromString("POLICY-PAYMENT")

    def fidaCardTokenName(identifier: ByteString): TokenName =
        Builtins.appendByteString(ByteString.fromString("CARD-"), identifier)

    val fidaCardStatusTokenName: TokenName = ByteString.fromString("CARD-STATUS")
}
