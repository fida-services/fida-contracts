package fida.contract.insurance.lifecycle

import fida.contract.Scripts
import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PolicyClaimPayment}
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyFunding
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PiggyBankDatum.{PBankFidaCard, PBankPremium}
import fida.contract.insurance.PolicyFundingRedeemer.{PolicyFundingCancel, PolicyFundingFundingComplete}
import fida.contract.testkit.{ScriptAssertions, ScriptEval}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.Result
import scalus.cardano.onchain.plutus.v1.{Interval, IntervalBound, IntervalBoundType, PolicyId, PosixTime, PubKeyHash, TokenName}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.{List, Option}

/** Direct-evaluation tests of the Funding lifecycle (port of
  * `Fida.Contract.Insurance.Lifecycle.Funding`), run through the full insurance policy validator.
  */
class FundingLifecycleSpec extends AnyFunSuite with ScriptAssertions {

    private val cs = insuranceId.cs
    private val otherCs: PolicyId = ByteString.fromHex("cd" * 28)
    private val policyAddress = scriptAddress(scriptHash(1))
    private val infoNft = (cs, Tokens.policyInfoTokenName, BigInt(1))
    private val cardStatus = (cs, Tokens.fidaCardStatusTokenName, BigInt(1))
    private val infoRef = outRef(1)

    private val funding: InsuranceInfo = sampleInsuranceInfo(state = Funding)
    private val startDate: PosixTime = beginningOfTime + days(2)
    private val onRisk: Data = funding.copy(iInfoState = OnRisk, iInfoStartDate = Option.Some(startDate)).toData
    private val afterStart: Interval = Interval.after(startDate + seconds(100))

    private val cancel = PolicyFunding(PolicyFundingCancel)
    private def complete(start: PosixTime = startDate) = PolicyFunding(PolicyFundingFundingComplete(start))

    private def run(datum: InsurancePolicyDatum, redeemer: InsurancePolicyRedeemer, ctx: ScriptContext): Result =
        ScriptEval.eval(
          Scripts.insurancePolicyValidator.program,
          insuranceId.toData,
          datum.toData,
          redeemer.toData,
          ctx.toData
        )

    /** Rejected, and `code` is the only error code in the logs: the one the Haskell validator reports. */
    private def assertRejectedWith(code: String)(result: Result): Unit =
        assertFailsWith(code)(result)
        val codes = result.logs.filter(_.startsWith("ERR-"))
        assert(codes == Seq(code), s"expected only $code, got ${codes.mkString("[", ", ", "]")}")

    private def withState(info: InsuranceInfo, state: InsurancePolicyState): Data =
        info.copy(iInfoState = state).toData

    private def infoOut(datum: Data): TxOut = txOut(policyAddress, value(2_000_000, infoNft), datum)

    private def soldCard(n: Int): Data = PBankFidaCard(true, fidaCardId(n), fidaCardValue, List.Nil).toData

    /** A piggy bank fida card UTxO, as created by `makePolicy` and updated by `BuyFidaCard`. */
    private def card(
        n: Int,
        datum: Data = null,
        status: (PolicyId, TokenName, BigInt) = cardStatus,
        ref: Int = 0
    ): TxInInfo =
        val d = scala.Option(datum).getOrElse(soldCard(n))
        txIn(outRef(100 + n, ref), txOut(scriptAddress(scriptHash(1000 + n)), value(fidaCardValue + 1_000_000, status), d))

    private val allSoldCards: Seq[TxInInfo] = (1 to 10).map(card(_))

    /** `allSoldCards` with the first card replaced by `replacement`. */
    private def firstCardReplacedBy(replacement: TxInInfo*): Seq[TxInInfo] = replacement ++ allSoldCards.tail

    private def ctx(
        info: InsurancePolicyDatum = funding,
        output: Seq[TxOut] = Seq(infoOut(onRisk)),
        cards: Seq[TxInInfo] = allSoldCards,
        validRange: Interval = afterStart,
        signers: Seq[PubKeyHash] = Seq(policyHolder),
        otherInputs: Seq[TxInInfo] = Seq.empty
    ): ScriptContext =
        spending(
          txInfo(
            inputs = txIn(infoRef, infoOut(info.toData)) +: otherInputs,
            referenceInputs = cards,
            outputs = output,
            validRange = validRange,
            signatories = signers
          ),
          infoRef
        )

    private def cancelCtx(
        info: InsuranceInfo = funding,
        output: Data = withState(funding, Cancelled),
        signers: Seq[PubKeyHash] = Seq(broker1)
    ): ScriptContext = ctx(info = info, output = Seq(infoOut(output)), cards = Seq.empty, signers = signers)

    // FundingCancel

    test("the policy authority cancels a Funding policy") {
        assertSucceeds(run(funding, cancel, cancelCtx()))
        assertSucceeds(run(funding, cancel, cancelCtx(signers = Seq(fidaSystem, policyHolder))))
    }

    test("FundingCancel cancels a policy in any state but Cancelled (Haskell quirk)") {
        for state <- Seq(Initiated, OnRisk, Expired) do
            val info = funding.copy(iInfoState = state, iInfoStartDate = Option.Some(startDate))
            assertSucceeds(run(info, cancel, cancelCtx(info = info, output = withState(info, Cancelled))))
    }

    test("ERR-FUND-1: cancelling requires the policy authority's signature") {
        for signers <- Seq(Seq.empty, Seq(policyHolder), Seq(fidaSystem, broker1)) do
            assertRejectedWith("ERR-FUND-1")(run(funding, cancel, cancelCtx(signers = signers)))
    }

    test("ERR-FUND-1 is reported before ERR-FUND-2") {
        assertRejectedWith("ERR-FUND-1")(
          run(funding, cancel, cancelCtx(output = withState(funding, Expired), signers = Seq(investor1)))
        )
    }

    test("ERR-FUND-2: cancelling must set the policy state to Cancelled") {
        for state <- Seq(Initiated, Funding, OnRisk, Expired) do
            assertRejectedWith("ERR-FUND-2")(run(funding, cancel, cancelCtx(output = withState(funding, state))))
        val missing = ctx(output = Seq.empty, cards = Seq.empty, signers = Seq(broker1))
        assertRejectedWith("ERR-FUND-2")(run(funding, cancel, missing))
    }

    test("ERR-FUND-0: FundingCancel requires an InsuranceInfo datum") {
        for datum <- Seq(samplePremiumPaymentInfo(), PolicyClaimPayment) do
            assertRejectedWith("ERR-FUND-0")(run(datum, cancel, ctx(info = datum, cards = Seq.empty, signers = Seq(broker1))))
    }

    // FundingComplete

    test("the policy holder completes funding once every fida card is sold") {
        assertSucceeds(run(funding, complete(), ctx()))
    }

    test("the policy authority may complete funding too") {
        assertSucceeds(run(funding, complete(), ctx(signers = Seq(broker1))))
    }

    test("selling more fida cards than required is accepted") {
        assertSucceeds(run(funding, complete(), ctx(cards = allSoldCards :+ card(11))))
    }

    test("a start date equal to an exclusive lower bound of the validity range is accepted") {
        val range = Interval(IntervalBound(IntervalBoundType.Finite(startDate), false), IntervalBound.posInf)
        assertSucceeds(run(funding, complete(), ctx(validRange = range)))
    }

    test("ERR-FUND-0: FundingComplete requires an InsuranceInfo in state Funding") {
        for state <- Seq(Initiated, OnRisk, Expired) do
            val info = funding.copy(iInfoState = state)
            assertRejectedWith("ERR-FUND-0")(run(info, complete(), ctx(info = info)))
        for datum <- Seq(samplePremiumPaymentInfo(), PolicyClaimPayment) do
            assertRejectedWith("ERR-FUND-0")(run(datum, complete(), ctx(info = datum)))
    }

    test("ERR-FUND-3: every fida card must be sold") {
        val cardSets = Seq(
          allSoldCards.tail,
          Seq.empty,
          firstCardReplacedBy(card(1, PBankFidaCard(false, fidaCardId(1), fidaCardValue, List.Nil).toData)),
          firstCardReplacedBy(card(2, ref = 1)),
          firstCardReplacedBy(card(1, status = (cs, Tokens.fidaCardStatusTokenName, BigInt(2)))),
          firstCardReplacedBy(card(1, status = (otherCs, Tokens.fidaCardStatusTokenName, BigInt(1)))),
          firstCardReplacedBy(card(1, status = (cs, Tokens.policyInfoTokenName, BigInt(1)))),
          firstCardReplacedBy(
            txIn(outRef(101), TxOut(scriptAddress(scriptHash(1001)), value(1_000_000, cardStatus), OutputDatum.OutputDatumHash(ByteString.fromHex("33" * 32))))
          ),
          firstCardReplacedBy(txIn(outRef(101), txOutNoDatum(scriptAddress(scriptHash(1001)), value(1_000_000, cardStatus))))
        )
        for cards <- cardSets do
            assertRejectedWith("ERR-FUND-3")(run(funding, complete(), ctx(cards = cards)))
    }

    test("ERR-FUND-3: only reference inputs count as sold fida cards") {
        assertRejectedWith("ERR-FUND-3")(
          run(funding, complete(), ctx(cards = allSoldCards.tail, otherInputs = Seq(allSoldCards.head)))
        )
    }

    test("ERR-FUND-3: fida card datums are decoded like fromBuiltinData, so malformed ones do not count") {
        val sold = Data.Constr(1, List.Nil)
        val id = Data.B(fidaCardId(1).id)
        val cardValue = Data.I(fidaCardValue)
        val noClaims = Data.List(List.Nil)
        val malformedDatums = Seq(
          Data.Constr(1, List(sold, id, cardValue, noClaims, Data.I(0))),
          Data.Constr(1, List(sold, id, cardValue)),
          Data.Constr(2, List(sold, id, cardValue, noClaims)),
          Data.Constr(0, List(sold, id, cardValue, noClaims)),
          Data.Constr(1, List(Data.Constr(1, List(Data.I(0))), id, cardValue, noClaims)),
          Data.Constr(1, List(Data.Constr(2, List.Nil), id, cardValue, noClaims)),
          Data.Constr(1, List(Data.I(1), id, cardValue, noClaims)),
          Data.Constr(1, List(sold, Data.I(1), cardValue, noClaims)),
          Data.Constr(1, List(sold, id, Data.B(ByteString.empty), noClaims)),
          Data.Constr(1, List(sold, id, cardValue, Data.List(List(Data.I(1))))),
          Data.Constr(1, List(sold, id, cardValue, Data.Map(List.Nil))),
          Data.Constr(1, List(sold, id, cardValue, Data.B(ByteString.empty))),
          Data.Constr(1, List(Data.B(ByteString.empty), id, cardValue, noClaims)),
          Data.Constr(1, List(Data.List(List.Nil), id, cardValue, noClaims)),
          Data.Constr(1, List(Data.Map(List.Nil), id, cardValue, noClaims)),
          Data.I(1),
          Data.B(fidaCardId(1).id),
          Data.List(List(sold, id, cardValue, noClaims)),
          Data.Map(List.Nil),
          PBankPremium(fidaCardValue, 0).toData
        )
        for (datum, i) <- malformedDatums.zipWithIndex do
            withClue(s"malformed fida card #$i: $datum\n") {
                assertRejectedWith("ERR-FUND-3")(run(funding, complete(), ctx(cards = firstCardReplacedBy(card(1, datum)))))
            }
    }

    test("sold fida cards with paid claims and any card value count") {
        val claims = List(ByteString.fromString("c1"), ByteString.fromString("c2"))
        val cards = firstCardReplacedBy(card(1, PBankFidaCard(true, fidaCardId(1), 0, claims).toData))
        assertSucceeds(run(funding, complete(), ctx(cards = cards)))
    }

    test("ERR-FUND-4: completing funding must set state OnRisk and the start date, changing nothing else") {
        val outputs = Seq(
          funding.copy(iInfoState = OnRisk, iInfoStartDate = Option.Some(startDate + 1)).toData,
          funding.copy(iInfoState = OnRisk).toData,
          funding.copy(iInfoStartDate = Option.Some(startDate)).toData,
          funding.copy(iInfoState = OnRisk, iInfoStartDate = Option.Some(startDate), iInfoFidaCardNumber = 9).toData
        )
        for output <- outputs do
            assertRejectedWith("ERR-FUND-4")(run(funding, complete(), ctx(output = Seq(infoOut(output)))))
        assertRejectedWith("ERR-FUND-4")(run(funding, complete(), ctx(output = Seq.empty)))
    }

    test("ERR-FUND-5: the start date must be before the validity range") {
        val ranges = Seq(
          Interval.after(startDate),
          Interval.after(startDate - 1),
          Interval.always,
          Interval.before(startDate + days(1))
        )
        for range <- ranges do
            assertRejectedWith("ERR-FUND-5")(run(funding, complete(), ctx(validRange = range)))
    }

    test("ERR-FUND-6: completing funding must be signed by the policy holder or the policy authority") {
        for signers <- Seq(Seq.empty, Seq(investor1), Seq(fidaSystem, broker1)) do
            assertRejectedWith("ERR-FUND-6")(run(funding, complete(), ctx(signers = signers)))
    }

    test("FundingComplete reports ERR-FUND-3, -4, -5 and -6 in that order") {
        val wrongOutput = Seq(infoOut(withState(funding, Funding)))
        assertRejectedWith("ERR-FUND-3")(
          run(funding, complete(), ctx(cards = Seq.empty, output = wrongOutput, validRange = Interval.always, signers = Seq.empty))
        )
        assertRejectedWith("ERR-FUND-4")(
          run(funding, complete(), ctx(output = wrongOutput, validRange = Interval.always, signers = Seq.empty))
        )
        assertRejectedWith("ERR-FUND-5")(run(funding, complete(), ctx(validRange = Interval.always, signers = Seq.empty)))
    }
}
