package fida.contract

import fida.contract.insurance.{InstallmentsInfo, InsuranceAuthority, PiggyBankDatum}
import fida.contract.insurance.InsuranceAuthority.*
import fida.contract.testkit.ContextBuilder.{outRef, pkh, spending, txInfo}
import fida.contract.testkit.Fixtures.{beginningOfTime, days}
import fida.contract.testkit.{ScriptEval, Uplc}
import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.v1.{
    Interval,
    IntervalBound,
    IntervalBoundType,
    PosixTime,
    PubKeyHash
}
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.compiler.Options
import scalus.uplc.{Constant, PlutusV2, Program, Term}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{to, toData}
import scalus.uplc.eval.Result

/** Tiny wrappers around the on-chain functions under test, compiled like the contracts. */
private object OnChainFunctions {
    given Options = Scripts.options

    /** `Utils.before(time, range)`. */
    lazy val before: PlutusV2[Data => Data => Boolean] =
        PlutusV2.compile((time: Data) =>
            (range: Data) => Utils.before(time.to[BigInt], range.to[Interval])
        )

    /** `PiggyBankDatum.unlockedPremiumToClaim(range, locked, installments, start)`. */
    lazy val unlockedPremiumToClaim: PlutusV2[Data => Data => Data => Data => BigInt] =
        PlutusV2.compile((range: Data) =>
            (locked: Data) =>
                (installments: Data) =>
                    (start: Data) =>
                        PiggyBankDatum.unlockedPremiumToClaim(
                          range.to[Interval],
                          locked.to[BigInt],
                          installments.to[InstallmentsInfo],
                          start.to[BigInt]
                        )
        )

    /** `InsuranceAuthority.isSignedByTheAuthority(ctx, authority)`. */
    lazy val isSignedByTheAuthority: PlutusV2[Data => Data => Boolean] =
        PlutusV2.compile((ctx: Data) =>
            (authority: Data) =>
                InsuranceAuthority.isSignedByTheAuthority(
                  ctx.to[ScriptContext],
                  authority.to[InsuranceAuthority]
                )
        )
}

/** Plain-Scala models of the Haskell functions, written from their Haskell definitions. */
private object HaskellModel {

    /** `compare` on `Extended`: `NegInf < Finite a < PosInf`. */
    private def compareExtended(a: IntervalBoundType, b: IntervalBoundType): Int =
        def rank(e: IntervalBoundType): Int = e match
            case IntervalBoundType.NegInf    => 0
            case IntervalBoundType.Finite(_) => 1
            case IntervalBoundType.PosInf    => 2
        (a, b) match
            case (IntervalBoundType.Finite(x), IntervalBoundType.Finite(y)) => x.compare(y)
            case _ => rank(a).compare(rank(b))

    /** `compare` on `LowerBound`: by point, then an inclusive lower bound is smaller than an
      * exclusive one.
      */
    private def compareLowerBounds(a: IntervalBound, b: IntervalBound): Int =
        compareExtended(a.boundType, b.boundType) match
            case 0 =>
                (a.isInclusive, b.isInclusive) match
                    case (true, false) => -1
                    case (false, true) => 1
                    case _             => 0
            case c => c

    /** `Interval.before h (Interval f _) = lowerBound h < f` where
      * `lowerBound h = LowerBound (Finite h) True`.
      */
    def before(h: PosixTime, range: Interval): Boolean =
        compareLowerBounds(IntervalBound(IntervalBoundType.Finite(h), true), range.from) < 0

    /** `unlockedPremiumToClaim`: one installment per due date strictly before the range, walking
      * the schedule until the first due date that is not; the whole locked amount once every
      * installment is due.
      */
    def unlockedPremiumToClaim(
        range: Interval,
        locked: BigInt,
        installments: InstallmentsInfo,
        start: PosixTime
    ): BigInt =
        val intervals = installments.paymentIntervals.toScalaList
        val dueDates = intervals.scanLeft(start)(_ + _).tail
        val due = dueDates.takeWhile(before(_, range)).size
        if due == intervals.size then locked else installments.amountPerInstallment * due

    /** `isSignedByTheAuthority`, with `signedBy = count id . map (txSignedBy txInfo)`. */
    def isSignedByTheAuthority(
        signatories: Seq[PubKeyHash],
        authority: InsuranceAuthority
    ): Boolean =
        def signedBy(pkhs: PList[PubKeyHash]): Int = pkhs.toScalaList.count(signatories.contains)
        authority match
            case SingleSign(pkh)        => signatories.contains(pkh)
            case AtLeastOneSign(pkhs)   => signedBy(pkhs) == 1
            case AllMustSign(pkhs)      => signedBy(pkhs) == pkhs.toScalaList.size
            case MajorityMustSign(pkhs) => 2 * signedBy(pkhs) > pkhs.toScalaList.size
}

/** The pure on-chain functions behave like their Haskell originals, run on the JVM and (when the
  * build compiles the contracts) as compiled UPLC.
  */
class OnChainFunctionsSpec extends AnyFunSuite with ScalaCheckPropertyChecks {

    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = PosInt(300))

    /** `onJvm`, the function's result as Scala, checked against `program` applied to `args` as UPLC
      * when the build compiles the contracts.
      */
    private def checkedOnChain[A](onJvm: A, program: => Program, args: Data*)(
        decode: Term => A
    ): A =
        if Uplc.available then
            val onUplc = ScriptEval.eval(program, args*) match
                case success: Result.Success => decode(success.term)
                case failure                 => fail(s"evaluation failed: $failure")
            assert(onUplc == onJvm, s"UPLC returned $onUplc, the JVM $onJvm")
        onJvm

    private def boolean(term: Term): Boolean = term match
        case Term.Const(Constant.Bool(b), _) => b
        case other                           => fail(s"expected a Boolean, got ${other.show}")

    private def integer(term: Term): BigInt = term match
        case Term.Const(Constant.Integer(n), _) => n
        case other                              => fail(s"expected an integer, got ${other.show}")

    private def before(h: PosixTime, range: Interval): Boolean =
        checkedOnChain(
          Utils.before(h, range),
          OnChainFunctions.before.program,
          h.toData,
          range.toData
        )(
          boolean
        )

    private def unlockedPremiumToClaim(
        range: Interval,
        locked: BigInt,
        installments: InstallmentsInfo,
        start: PosixTime
    ): BigInt =
        checkedOnChain(
          PiggyBankDatum.unlockedPremiumToClaim(range, locked, installments, start),
          OnChainFunctions.unlockedPremiumToClaim.program,
          range.toData,
          locked.toData,
          installments.toData,
          start.toData
        )(integer)

    private def isSignedByTheAuthority(
        signatories: Seq[PubKeyHash],
        authority: InsuranceAuthority
    ): Boolean =
        val ctx = spending(txInfo(signatories = signatories), outRef(1))
        checkedOnChain(
          InsuranceAuthority.isSignedByTheAuthority(ctx, authority),
          OnChainFunctions.isSignedByTheAuthority.program,
          ctx.toData,
          authority.toData
        )(boolean)

    private def lower(bound: IntervalBoundType, inclusive: Boolean): Interval =
        Interval(IntervalBound(bound, inclusive), IntervalBound.posInf)

    private def ada(n: Int): BigInt = BigInt(n) * 1_000_000

    // Interval.before -------------------------------------------------------------------------

    test("before: true iff the time is strictly earlier than the range's lower bound") {
        import IntervalBoundType.*
        val t = beginningOfTime
        val cases = Seq(
          ("a later inclusive lower bound", t, Interval.after(t + 1), true),
          ("an inclusive lower bound at the same time", t, Interval.after(t), false),
          (
            "an exclusive lower bound at the same time",
            t,
            lower(Finite(t), inclusive = false),
            true
          ),
          ("an earlier inclusive lower bound", t, Interval.after(t - 1), false),
          ("an earlier exclusive lower bound", t, lower(Finite(t - 1), inclusive = false), false),
          ("a later exclusive lower bound", t, lower(Finite(t + 1), inclusive = false), true),
          ("an inclusive -inf lower bound", t, Interval.always, false),
          ("an exclusive -inf lower bound", t, lower(NegInf, inclusive = false), false),
          ("an inclusive +inf lower bound", t, Interval.never, true),
          ("an exclusive +inf lower bound", t, lower(PosInf, inclusive = false), true),
          ("a bounded range (the upper bound is ignored)", t, Interval.between(t + 1, t + 2), true),
          ("an empty range (the upper bound is ignored)", t, Interval.between(t + 1, t - 9), true),
          ("a range without lower bound", t, Interval.before(t + 10), false),
          ("negative times", BigInt(-5), Interval.after(-4), true)
        )
        for (description, h, range, expected) <- cases do
            assert(HaskellModel.before(h, range) == expected, s"model: $description")
            assert(before(h, range) == expected, description)
    }

    /** A bound type relative to a time `t`: infinite, or finite within 2 ms of `t`. */
    private val genBoundType: Gen[BigInt => IntervalBoundType] = Gen.frequency(
      1 -> Gen.const((_: BigInt) => IntervalBoundType.NegInf),
      1 -> Gen.const((_: BigInt) => IntervalBoundType.PosInf),
      6 -> Gen.choose(-2, 2).map(delta => (t: BigInt) => IntervalBoundType.Finite(t + delta))
    )

    /** A range whose bounds are near `t` (or infinite), to hit the boundary cases often. */
    private def genRangeNear(t: BigInt): Gen[Interval] = for
        from <- genBoundType
        fromInclusive <- Gen.oneOf(true, false)
        to <- genBoundType
        toInclusive <- Gen.oneOf(true, false)
    yield Interval(IntervalBound(from(t), fromInclusive), IntervalBound(to(t), toInclusive))

    private val genTime: Gen[BigInt] = Gen.oneOf(
      Gen.choose(-1000L, 1000L).map(BigInt(_)),
      Gen.choose(0L, 4_000_000_000_000L).map(BigInt(_)),
      Gen.choose(Long.MinValue, Long.MaxValue).map(BigInt(_) * 1000)
    )

    test("before agrees with Haskell's `lowerBound h < ivFrom range` (property)") {
        forAll(genTime.flatMap(t => genRangeNear(t).map((t, _)))) { (t, range) =>
            assert(before(t, range) == HaskellModel.before(t, range))
        }
        forAll(genTime, genTime.flatMap(genRangeNear)) { (t, range) =>
            assert(before(t, range) == HaskellModel.before(t, range))
        }
    }

    // unlockedPremiumToClaim ------------------------------------------------------------------

    /** The sample policy: 3 payment intervals of 90 days, 5 ADA per installment, 20 ADA locked (the
      * first of the 4 installments is paid when the policy starts).
      */
    private val start = beginningOfTime
    private val sampleInstallments = InstallmentsInfo(PList(days(90), days(90), days(90)), ada(5))
    private val sampleLocked = ada(20)

    test(
      "unlockedPremiumToClaim: 0, 5, 10 ADA before/after each due date, all 20 ADA once all are due"
    ) {
        import IntervalBoundType.*
        val due1 = start + days(90)
        val due2 = start + days(180)
        val due3 = start + days(270)
        val cases = Seq(
          ("before the start", Interval.after(start - days(1)), ada(0)),
          ("at the start", Interval.after(start), ada(0)),
          ("one day before the first due date", Interval.after(due1 - days(1)), ada(0)),
          ("at the first due date", Interval.after(due1), ada(0)),
          (
            "right after the first due date (exclusive)",
            lower(Finite(due1), inclusive = false),
            ada(5)
          ),
          ("1 ms after the first due date", Interval.after(due1 + 1), ada(5)),
          ("at the second due date", Interval.after(due2), ada(5)),
          ("1 ms after the second due date", Interval.after(due2 + 1), ada(10)),
          (
            "in a bounded range after the second due date",
            Interval.between(due2 + 1, due3 + days(9)),
            ada(10)
          ),
          ("at the third due date", Interval.after(due3), ada(10)),
          // never 15 ADA (3 x 5): once every installment is due, all the locked premium is
          ("1 ms after the third due date", Interval.after(due3 + 1), sampleLocked),
          ("long after the third due date", Interval.after(due3 + days(1000)), sampleLocked),
          ("without lower bound (-inf)", Interval.always, ada(0)),
          ("with a +inf lower bound", Interval.never, sampleLocked)
        )
        for (description, range, expected) <- cases do
            assert(
              HaskellModel.unlockedPremiumToClaim(
                range,
                sampleLocked,
                sampleInstallments,
                start
              ) == expected,
              s"model: $description"
            )
            assert(
              unlockedPremiumToClaim(range, sampleLocked, sampleInstallments, start) == expected,
              description
            )
    }

    test("unlockedPremiumToClaim: an empty schedule unlocks everything at once") {
        val range = Interval.after(start - days(1))
        assert(
          unlockedPremiumToClaim(
            range,
            sampleLocked,
            InstallmentsInfo(PList.Nil, ada(5)),
            start
          ) == sampleLocked
        )
    }

    /** Any schedule (including negative or zero intervals), with ranges near its due dates. */
    private val genSchedule: Gen[(Interval, BigInt, InstallmentsInfo, PosixTime)] = for
        start <- genTime
        n <- Gen.choose(0, 5)
        intervals <- Gen.listOfN(
          n,
          Gen.oneOf(Gen.choose(-days(3), days(100)), Gen.choose(-2L, 2L).map(BigInt(_)))
        )
        perInstallment <- Gen.choose(-ada(10), ada(10))
        locked <- Gen.choose(-ada(10), ada(100))
        dueDates = intervals.scanLeft(start)(_ + _)
        near <- Gen.oneOf(dueDates)
        range <- genRangeNear(near)
    yield (range, locked, InstallmentsInfo(PList.from(intervals), perInstallment), start)

    test("unlockedPremiumToClaim agrees with the Haskell definition (property)") {
        forAll(genSchedule) { (range, locked, installments, start) =>
            assert(
              unlockedPremiumToClaim(range, locked, installments, start) ==
                  HaskellModel.unlockedPremiumToClaim(range, locked, installments, start)
            )
        }
    }

    /** A well-formed policy: positive intervals, and at least one installment per interval locked.
      */
    private val genPolicy: Gen[(BigInt, InstallmentsInfo, PosixTime)] = for
        start <- Gen.choose(0L, 4_000_000_000_000L).map(BigInt(_))
        n <- Gen.choose(1, 6)
        intervals <- Gen.listOfN(n, Gen.choose(BigInt(1), days(120)))
        perInstallment <- Gen.choose(BigInt(0), ada(10))
        extra <- Gen.choose(BigInt(0), ada(10))
    yield (
      perInstallment * (n + 1) + extra,
      InstallmentsInfo(PList.from(intervals), perInstallment),
      start
    )

    test(
      "unlockedPremiumToClaim is monotonic in time and never above the locked amount (property)"
    ) {
        forAll(genPolicy, Gen.choose(-days(10), days(800)), Gen.choose(-days(10), days(800))) {
            case ((locked, installments, start), d1, d2) =>
                val t1 = start + d1.min(d2)
                val t2 = start + d1.max(d2)
                def unlockedFrom(from: IntervalBound) = unlockedPremiumToClaim(
                  Interval(from, IntervalBound.posInf),
                  locked,
                  installments,
                  start
                )
                val earlier = unlockedFrom(IntervalBound.finiteInclusive(t1))
                val later = unlockedFrom(IntervalBound.finiteInclusive(t2))
                val laterExclusive = unlockedFrom(IntervalBound.finiteExclusive(t2))
                assert(BigInt(0) <= earlier && earlier <= later && later <= laterExclusive)
                assert(laterExclusive <= locked)
                val intervals = installments.paymentIntervals.toScalaList
                if t1 <= start + intervals.head then assert(earlier == 0)
                if t2 > start + intervals.sum then assert(later == locked)
        }
    }

    // isSignedByTheAuthority ------------------------------------------------------------------

    private val a = pkh(101)
    private val b = pkh(102)
    private val c = pkh(103)
    private val d = pkh(104)
    private val outsider = pkh(199)

    test("isSignedByTheAuthority: all four strategies, with the Haskell semantics") {
        val cases = Seq(
          (SingleSign(a), Seq(a), true),
          (SingleSign(a), Seq(b, a, c), true),
          (SingleSign(a), Seq(b), false),
          (SingleSign(a), Seq(), false),
          // AtLeastOneSign is satisfied by EXACTLY one of the listed keys
          (AtLeastOneSign(PList(a, b)), Seq(a), true),
          (AtLeastOneSign(PList(a, b)), Seq(outsider, b), true),
          (AtLeastOneSign(PList(a, b)), Seq(a, b), false),
          (AtLeastOneSign(PList(a, b, c)), Seq(a, b, c), false),
          (AtLeastOneSign(PList(a, b)), Seq(outsider), false),
          (AtLeastOneSign(PList(a, b)), Seq(), false),
          (AtLeastOneSign(PList()), Seq(a), false),
          // AllMustSign
          (AllMustSign(PList(a, b, c)), Seq(c, b, a), true),
          (AllMustSign(PList(a, b, c)), Seq(a, b, c, outsider), true),
          (AllMustSign(PList(a, b, c)), Seq(a, b, outsider), false),
          (AllMustSign(PList(a)), Seq(), false),
          (AllMustSign(PList()), Seq(), true),
          // MajorityMustSign: strictly more than half of the listed keys
          (MajorityMustSign(PList(a, b, c)), Seq(a, b), true),
          (MajorityMustSign(PList(a, b, c)), Seq(c), false),
          (MajorityMustSign(PList(a, b, c, d)), Seq(a, b), false),
          (MajorityMustSign(PList(a, b, c, d)), Seq(a, b, d), true),
          (MajorityMustSign(PList(a)), Seq(a), true),
          (MajorityMustSign(PList(a, b)), Seq(a), false),
          (MajorityMustSign(PList()), Seq(a), false)
        )
        for (authority, signatories, expected) <- cases do
            assert(
              HaskellModel.isSignedByTheAuthority(signatories, authority) == expected,
              s"model: $authority by $signatories"
            )
            assert(
              isSignedByTheAuthority(signatories, authority) == expected,
              s"$authority by $signatories"
            )
    }

    test("isSignedByTheAuthority counts a key listed twice once per occurrence") {
        val cases = Seq(
          (AtLeastOneSign(PList(a, a)), Seq(a), false), // counted twice: not exactly one
          (AtLeastOneSign(PList(a, a, b)), Seq(b), true),
          (AllMustSign(PList(a, a, b)), Seq(a, b), true),
          (AllMustSign(PList(a, a, b)), Seq(a), false),
          (MajorityMustSign(PList(a, a, b)), Seq(a), true), // 2 of 3
          (MajorityMustSign(PList(a, b, b)), Seq(a), false), // 1 of 3
          (MajorityMustSign(PList(a, a, b, c)), Seq(a), false), // 2 of 4
          // a key signing twice is still one signatory
          (AtLeastOneSign(PList(a, b)), Seq(a, a), true),
          (MajorityMustSign(PList(a, b, c)), Seq(a, a), false)
        )
        for (authority, signatories, expected) <- cases do
            assert(
              HaskellModel.isSignedByTheAuthority(signatories, authority) == expected,
              s"model: $authority by $signatories"
            )
            assert(
              isSignedByTheAuthority(signatories, authority) == expected,
              s"$authority by $signatories"
            )
    }

    private val keys = Seq(a, b, c, d, outsider)
    private val genKeys: Gen[PList[PubKeyHash]] =
        Gen.choose(0, 6).flatMap(Gen.listOfN(_, Gen.oneOf(keys))).map(PList.from)
    private val genAuthority: Gen[InsuranceAuthority] = Gen.oneOf(
      Gen.oneOf(keys).map(SingleSign(_)),
      genKeys.map(AtLeastOneSign(_)),
      genKeys.map(AllMustSign(_)),
      genKeys.map(MajorityMustSign(_))
    )

    test("isSignedByTheAuthority agrees with the Haskell definition (property)") {
        forAll(genAuthority, Gen.someOf(keys)) { (authority, signatories) =>
            assert(
              isSignedByTheAuthority(signatories.toSeq, authority) ==
                  HaskellModel.isSignedByTheAuthority(signatories.toSeq, authority)
            )
        }
    }
}
