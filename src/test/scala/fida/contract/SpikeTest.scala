package fida.contract

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{TxBuilder, TxBuilderException}
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.utils.await
import scalus.cardano.onchain.plutus.v1.{TxId, TxOutRef}

class SpikeTest extends AnyFunSuite {
    private val alice = Party.Alice
    private val aliceAddr = alice.address(Network.Mainnet)

    private def setup() =
        val emulator = Emulator.withAddresses(Seq(aliceAddr, aliceAddr, aliceAddr))
        val sorted = emulator.utxos.toSeq.sortBy(_._1.index)
        (emulator, sorted)

    test("mint with the InsuranceId policy in the emulator") {
        val (emulator, utxos) = setup()
        given env: CardanoInfo = emulator.cardanoInfo
        val (input, output) = utxos.head
        val ref = TxOutRef(TxId(input.transactionId), BigInt(input.index))
        val script = Scripts.insuranceIdMintingPolicyScript(ref)
        println(s"script size: ${script.script.size} hash: ${script.scriptHash.toHex} version: ${Scripts.insuranceIdMintingPolicy.program.version}")
        val tx = TxBuilder(env)
            .spend(Utxo(input, output))
            .mint(script, Map(AssetName(ByteString.fromString("POLICY-INFO")) -> 1L), ())
            .complete(emulator, aliceAddr)
            .await()
            .sign(alice.signer)
            .transaction
        val res = emulator.submit(tx).await()
        assert(res.isRight, res)
    }

    test("minting without spending the reference fails with ERR-IID-0") {
        val (emulator, utxos) = setup()
        given env: CardanoInfo = emulator.cardanoInfo
        val (input, _) = utxos.head
        val ref = TxOutRef(TxId(input.transactionId), BigInt(input.index + 7))
        val script = Scripts.insuranceIdMintingPolicyScript(ref)
        val err = intercept[TxBuilderException] {
            TxBuilder(env)
                .mint(script, Map(AssetName(ByteString.fromString("POLICY-INFO")) -> 1L), ())
                .complete(emulator, aliceAddr)
                .await()
        }
        println(s"${err.getClass.getName}: ${err.getMessage.take(300)}")
        err match
            case e: TxBuilderException.BalancingException => println(s"script logs: ${e.scriptLogs}")
            case _ => ()
    }
}
