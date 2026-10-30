package fida.contract.toolbox

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.{Emulator, NodeSubmitError, SubmitError}
import scalus.cardano.txbuilder.{TxBuilder, TxBuilderException}
import scalus.testing.kit.Party
import scalus.uplc.builtin.{Data, FromData}
import scalus.uplc.builtin.Data.fromData
import scalus.utils.await
import scalus.cardano.onchain.plutus.v1

import java.time.Instant
import scala.util.Try

/** A transaction rejected while being built (script evaluation during balancing) or by the
  * emulator's ledger rules. `logs` holds the script trace messages, e.g. `ERR-INIT-1`.
  */
final case class TxFailure(message: String, logs: Seq[String]) extends Exception(message)

/** A wallet: a deterministic test key pair. */
final case class User(name: String, party: Party) {
    def address(using env: CardanoInfo): Address = party.address(env.network)
    def addrKeyHash: AddrKeyHash = party.addrKeyHash
    def pkh: v1.PubKeyHash = v1.PubKeyHash(party.addrKeyHash)
    override def toString: String = name
}

/** A UTxO together with its decoded inline datum (plutus-simple-model's `TxBox`). */
final case class Box[D](utxo: Utxo, datum: D) {
    def value: Value = utxo.output.value
    def address: Address = utxo.output.address
}

/** One emulator session: the Scala counterpart of plutus-simple-model's `Run` monad.
  *
  * Every user is funded with 20 000 ADA (split over three UTxOs so one can serve as collateral).
  * As in plutus-simple-model, each submitted transaction lands in its own block: the slot advances
  * by one after every successful submission. Slots last one second.
  */
final class Run {
    import Run.*

    val users: Users = Users.all

    val emulator: Emulator = Emulator(
      initialUtxos = initialUtxos(users),
      initialContext = Context.testMainnet(startSlot)
    )

    given env: CardanoInfo = emulator.cardanoInfo

    /** A transaction builder for this session. */
    def txBuilder: TxBuilder = TxBuilder(env)

    // ---------------------------------------------------------------- time

    def currentSlot: Long = emulator.currentSlotSync

    /** POSIX time (ms) of the current slot. */
    def currentTime: v1.PosixTime = BigInt(env.slotConfig.slotToTime(currentSlot))

    def waitNSlots(n: Long): Unit = emulator.tick(n)

    /** Waits until the slot containing `time`. */
    def waitUntil(time: v1.PosixTime): Unit =
        val slot = env.slotConfig.timeToSlot(time.toLong)
        if slot > currentSlot then emulator.setSlot(slot)

    def instant(time: v1.PosixTime): Instant = Instant.ofEpochMilli(time.toLong)

    // ---------------------------------------------------------------- UTxOs

    def utxosAt(address: Address): Seq[Utxo] =
        emulator.utxos.toSeq.collect { case (in, out) if out.address == address => Utxo(in, out) }
            .sortBy(u => (u.input.transactionId.toHex, u.input.index))

    /** UTxOs at `address` whose inline datum decodes as `D`. */
    def boxesAt[D: FromData](address: Address): Seq[Box[D]] =
        utxosAt(address).flatMap(u =>
            u.output.inlineDatum.flatMap(d => Try(fromData[D](d)).toOption).map(Box(u, _))
        )

    /** The first box at `address` satisfying `p` (plutus-simple-model's `withBox`). */
    def box[D: FromData](address: Address, what: String)(p: Box[D] => Boolean): Box[D] =
        boxesAt[D](address).find(p).getOrElse(throw new NoSuchElementException(s"No $what box found"))

    /** The user's UTxOs holding a positive amount of the given asset. */
    def utxosWithAsset(user: User, policyId: PolicyId, assetName: AssetName): Seq[Utxo] =
        utxosAt(user.address).filter(_.output.value.assets.assets.get(policyId).exists(_.get(assetName).exists(_ > 0)))

    // ---------------------------------------------------------------- submission

    /** Balances `builder` with `signer`'s funds, adds `signer` as required signer (so it appears in
      * `txInfoSignatories`, like plutus-simple-model's `submitTx`), signs and submits it.
      */
    def submitTx(signer: User, builder: TxBuilder): TransactionHash =
        val completed =
            try builder.requireSignature(signer.addrKeyHash).complete(emulator, signer.address).await()
            catch case e: TxBuilderException => throw failureOf(e)
        val tx = completed.sign(signer.party.signer).transaction
        emulator.submit(tx).await() match
            case Right(hash) =>
                emulator.tick(1)
                hash
            case Left(error) => throw failureOf(error)
}

object Run {

    /** Friday, 1 March 2024 12:12:12 UTC (the Haskell toolbox's `beginningOfTime`). */
    val beginningOfTime: Long = 1709295132000L

    private val startSlot: Long = SlotConfig.mainnet.timeToSlot(beginningOfTime)

    private val genesis: TransactionHash =
        TransactionHash.fromByteString(scalus.uplc.builtin.ByteString.fromHex("0" * 64))

    private def initialUtxos(users: Users): Utxos =
        users.toSeq.zipWithIndex.flatMap { (user, i) =>
            val address = user.party.address(scalus.cardano.address.Network.Mainnet)
            Seq(19_900L, 50L, 50L).zipWithIndex.map { (ada, j) =>
                TransactionInput(genesis, i * 3 + j) -> TransactionOutput(address, Value.ada(ada))
            }
        }.toMap

    private def failureOf(e: TxBuilderException): TxFailure = e match
        case b: TxBuilderException.BalancingException =>
            TxFailure(b.getMessage, b.scriptLogs.getOrElse(Seq.empty))
        case other => TxFailure(other.getMessage, Seq.empty)

    private def failureOf(e: SubmitError): TxFailure = e match
        case s: NodeSubmitError.ScriptFailure => TxFailure(s.message, s.logs)
        case other                            => TxFailure(other.message, Seq.empty)
}
