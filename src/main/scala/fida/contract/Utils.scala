package fida.contract

import scalus.compiler.Compile
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.cardano.onchain.plutus.v1.{
    Interval,
    IntervalBoundType,
    PolicyId,
    PosixTime,
    PubKeyHash,
    ScriptPurpose,
    TokenName,
    Value
}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInInfo, TxInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.*

/** On-chain helpers.
  *
  * Port of `Fida.Contract.Utils` plus the handful of `plutus-ledger-api` functions the Haskell
  * contracts relied on (`txSignedBy`, `findOwnInput`, `getContinuingOutputs`, `valueSpent`,
  * `Interval.before`). Each helper keeps the exact semantics of its Haskell counterpart.
  */
@Compile
object Utils {

    /** `traceIfFalse msg b`: when `b` is false, logs `msg` and returns false. */
    inline def traceIfFalse(msg: String, cond: Boolean): Boolean =
        if cond then true else Builtins.trace(msg)(false)

    /** `traceError msg`: logs `msg` and fails the script. */
    inline def traceError(msg: String): Nothing = fail(msg)

    /** PlutusTx `check`: turns a validation result into success, or failure with PlutusTx's `PT5`
      * ("check input is False") code.
      */
    inline def check(cond: Boolean): Unit = if cond then () else fail("PT5")

    /** `valueOf v cs tn`: quantity of the given asset (0 when absent). */
    inline def valueOf(v: Value, cs: PolicyId, tn: TokenName): BigInt = v.quantityOf(cs, tn)

    /** `lovelaceValueOf v`: quantity of lovelace. */
    def lovelaceValueOf(v: Value): BigInt = v.quantityOf(ByteString.empty, ByteString.empty)

    /** Sum of the lovelace held by the given outputs (`lovelaceValueOf . mconcat`). */
    def lovelaceOf(outputs: List[TxOut]): BigInt =
        outputs.foldLeft(BigInt(0))((acc, out) => acc + lovelaceValueOf(out.value))

    /** `fromSingleton`: `Some(a)` for a one-element list, `None` otherwise. */
    def fromSingleton[A](xs: List[A]): Option[A] = xs match
        case List.Cons(a, List.Nil) => Option.Some(a)
        case _                      => Option.None

    /** `unsafeFromSingleton' err`: the single element, or fails with `err.1` (empty) or `err.2`
      * (more than one element).
      */
    def unsafeFromSingletonOr[A](err: String, xs: List[A]): A = xs match
        case List.Cons(a, List.Nil) => a
        case List.Nil               => traceError(Builtins.appendString(err, ".1"))
        case _                      => traceError(Builtins.appendString(err, ".2"))

    /** `traceIfNotSingleton msg`: the single element, or fails with `msg`. */
    def traceIfNotSingleton[A](msg: String, xs: List[A]): A = xs match
        case List.Cons(a, List.Nil) => a
        case _                      => traceError(msg)

    /** `txSignedBy`: whether the transaction is signed by the given key. */
    def txSignedBy(txInfo: TxInfo, pkh: PubKeyHash): Boolean =
        txInfo.signatories.exists(_ === pkh)

    /** `findOwnInput`: the input being validated (spending scripts only). */
    def findOwnInput(ctx: ScriptContext): Option[TxInInfo] = ctx.purpose match
        case ScriptPurpose.Spending(ref) => ctx.txInfo.inputs.find(_.outRef === ref)
        case _                           => Option.None

    /** `getContinuingOutputs`: outputs paying to the same address as the input being validated. */
    def getContinuingOutputs(ctx: ScriptContext): List[TxOut] = findOwnInput(ctx) match
        case Option.Some(input) => ctx.txInfo.outputs.filter(_.address === input.resolved.address)
        case Option.None        => traceError("Lf")

    /** `valueOf (valueSpent txInfo) cs tn`: total quantity of an asset across all inputs. */
    def valueSpentOf(txInfo: TxInfo, cs: PolicyId, tn: TokenName): BigInt =
        txInfo.inputs.foldLeft(BigInt(0))((acc, in) => acc + in.resolved.value.quantityOf(cs, tn))

    /** `Plutus.V1.Ledger.Interval.before h range`: `h` is strictly earlier than every point of
      * `range`, i.e. `lowerBound h < ivFrom range`, where an inclusive lower bound is smaller than
      * an exclusive one at the same point.
      */
    def before(h: PosixTime, range: Interval): Boolean = range.from.boundType match
        case IntervalBoundType.NegInf    => false
        case IntervalBoundType.PosInf    => true
        case IntervalBoundType.Finite(t) =>
            if h < t then true
            else if h === t then !range.from.isInclusive
            else false

    /** Inline datums of `outputs` that hold exactly one `(cs, tn)` token. */
    def inlineDatumsWithToken(
        outputs: List[TxOut],
        cs: PolicyId,
        tn: TokenName
    ): List[Data] =
        outputs.filterMap { out =>
            out.datum match
                case OutputDatum.OutputDatum(d) =>
                    if out.value.quantityOf(cs, tn) === BigInt(1) then Option.Some(d)
                    else Option.None
                case _ => Option.None
        }

    /** Outputs together with their inline datum, for outputs holding exactly one `(cs, tn)` token
      * (Haskell `outputs'` before decoding).
      */
    def outputsWithToken(
        outputs: List[TxOut],
        cs: PolicyId,
        tn: TokenName
    ): List[(TxOut, Data)] =
        outputs.filterMap { out =>
            out.datum match
                case OutputDatum.OutputDatum(d) =>
                    if out.value.quantityOf(cs, tn) === BigInt(1) then Option.Some((out, d))
                    else Option.None
                case _ => Option.None
        }

    /** `untypedOutputDatum cs sc tn`: inline datum of the single continuing output holding the
      * `(cs, tn)` token; `None` when there is not exactly one such output.
      */
    def untypedOutputDatum(cs: PolicyId, ctx: ScriptContext, tn: TokenName): Option[Data] =
        fromSingleton(inlineDatumsWithToken(getContinuingOutputs(ctx), cs, tn))

    /** `untypedOutput cs sc tn`: the single continuing output holding the `(cs, tn)` token (datum
      * not required).
      */
    def untypedOutput(cs: PolicyId, ctx: ScriptContext, tn: TokenName): Option[TxOut] =
        fromSingleton(
          getContinuingOutputs(ctx).filter(_.value.quantityOf(cs, tn) === BigInt(1))
        )

    /** Reference inputs holding exactly one `(cs, tn)` token, with their inline datum. */
    def referenceOutputs(cs: PolicyId, ctx: ScriptContext, tn: TokenName): List[(TxOut, Data)] =
        outputsWithToken(ctx.txInfo.referenceInputs.map(_.resolved), cs, tn)

    /** Inline datums of reference inputs holding exactly one `(cs, tn)` token. */
    def referenceDatums(cs: PolicyId, ctx: ScriptContext, tn: TokenName): List[Data] =
        inlineDatumsWithToken(ctx.txInfo.referenceInputs.map(_.resolved), cs, tn)
}
