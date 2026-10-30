package fida.contract.insurance.lifecycle

import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PolicyClaimPayment}
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyOnRisk
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PolicyOnRiskRedeemer.*
import fida.contract.testkit.{OnChainScript, Outcome, ScriptAssertions, ScriptEval}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Interval, PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.{List, Option}

/** Direct-evaluation tests of the OnRisk lifecycle (port of
  * `Fida.Contract.Insurance.Lifecycle.OnRisk`), run through the full insurance policy validator.
  */
class OnRiskLifecycleSpec extends AnyFunSuite with ScriptAssertions {

    private val cs = insuranceId.cs
    private val policyAddress = scriptAddress(scriptHash(1))
    private val infoNft = (cs, Tokens.policyInfoTokenName, BigInt(1))
    private val infoRef = outRef(1)
    private val paymentRef = outRef(7)

    private val startDate: PosixTime = beginningOfTime
    private val claim: ClaimInfo = sampleClaim
    private val acceptedClaim: ClaimInfo = sampleClaim.copy(claimAccepted = true)
    private val deadline: PosixTime =
        claim.claimDate + seconds(100) // claim date + iInfoClaimTimeToLive

    private def policy(
        claim: Option[ClaimInfo] = Option.None,
        state: InsurancePolicyState = OnRisk
    ): InsuranceInfo =
        sampleInsuranceInfo(state = state, startDate = Option.Some(startDate), claim = claim)

    private val noClaim = policy()
    private val withClaim = policy(Option.Some(claim))
    private val withAcceptedClaim = policy(Option.Some(acceptedClaim))

    private def run(
        datum: InsurancePolicyDatum,
        redeemer: PolicyOnRiskRedeemer,
        ctx: ScriptContext
    ): Outcome =
        ScriptEval.eval(
          OnChainScript.insurancePolicyValidator,
          insuranceId.toData,
          datum.toData,
          PolicyOnRisk(redeemer).toData,
          ctx.toData
        )

    /** Rejected, and `code` is the only error code in the logs: the one the Haskell validator
      * reports.
      */
    private def assertRejectedWith(code: String)(result: Outcome): Unit =
        assertFailsWith(code)(result)
        val codes = result.logs.filter(_.startsWith("ERR-"))
        assert(codes == Seq(code), s"expected only $code, got ${codes.mkString("[", ", ", "]")}")

    private def infoOut(datum: Data): TxOut = txOut(policyAddress, value(2_000_000, infoNft), datum)

    /** Spends the policy info holding `info` and recreates it with `output` (when given). */
    private def ctx(
        info: InsurancePolicyDatum,
        output: scala.Option[Data],
        signers: Seq[PubKeyHash] = Seq.empty,
        validRange: Interval = Interval.always
    ): ScriptContext =
        spending(
          txInfo(
            inputs = Seq(txIn(infoRef, infoOut(info.toData))),
            outputs = output.map(infoOut).toSeq,
            validRange = validRange,
            signatories = signers
          ),
          infoRef
        )

    // CreateClaim

    private val afterClaimDate = Interval.after(claim.claimDate + seconds(5))

    test("the policy holder creates a claim") {
        val c = ctx(noClaim, Some(withClaim.toData), Seq(policyHolder), afterClaimDate)
        assertSucceeds(run(noClaim, PolicyOnRiskCreateClaim(claim), c))
    }

    test("creating a claim replaces an existing, even accepted, claim (Haskell quirk)") {
        val c = ctx(withAcceptedClaim, Some(withClaim.toData), Seq(policyHolder), afterClaimDate)
        assertSucceeds(run(withAcceptedClaim, PolicyOnRiskCreateClaim(claim), c))
    }

    test("ERR-ON-RISK-0: the output must hold the new claim") {
        val outputs = Seq(
          Some(noClaim.toData),
          Some(withAcceptedClaim.toData),
          Some(policy(Option.Some(claim), state = Funding).toData),
          Some(policy(Option.Some(claim.copy(claimAmount = claim.claimAmount + 1))).toData),
          None
        )
        for output <- outputs do
            val c = ctx(noClaim, output, Seq(policyHolder), afterClaimDate)
            assertRejectedWith("ERR-ON-RISK-0")(run(noClaim, PolicyOnRiskCreateClaim(claim), c))
    }

    test("ERR-ON-RISK-0: a new claim must not be accepted") {
        val c = ctx(noClaim, Some(withAcceptedClaim.toData), Seq(policyHolder), afterClaimDate)
        assertRejectedWith("ERR-ON-RISK-0")(run(noClaim, PolicyOnRiskCreateClaim(acceptedClaim), c))
    }

    test("ERR-ON-RISK-0: the claim date must be before the validity range") {
        val ranges = Seq(
          Interval.after(claim.claimDate),
          Interval.after(claim.claimDate - 1),
          Interval.always
        )
        for range <- ranges do
            val c = ctx(noClaim, Some(withClaim.toData), Seq(policyHolder), range)
            assertRejectedWith("ERR-ON-RISK-0")(run(noClaim, PolicyOnRiskCreateClaim(claim), c))
    }

    test("ERR-ON-RISK-1: creating a claim must be signed by the policy holder") {
        for signers <- Seq(Seq.empty, Seq(broker1), Seq(investor1)) do
            val c = ctx(noClaim, Some(withClaim.toData), signers, afterClaimDate)
            assertRejectedWith("ERR-ON-RISK-1")(run(noClaim, PolicyOnRiskCreateClaim(claim), c))
    }

    test("ERR-ON-RISK-0 is reported before ERR-ON-RISK-1") {
        val c = ctx(noClaim, Some(noClaim.toData), Seq.empty, afterClaimDate)
        assertRejectedWith("ERR-ON-RISK-0")(run(noClaim, PolicyOnRiskCreateClaim(claim), c))
    }

    test("ERR-ON-RISK-13: claims can only be created for an InsuranceInfo in state OnRisk") {
        for state <- Seq(Initiated, Funding, Expired) do
            val info = policy(state = state)
            val c = ctx(
              info,
              Some(policy(Option.Some(claim), state).toData),
              Seq(policyHolder),
              afterClaimDate
            )
            assertRejectedWith("ERR-ON-RISK-13")(run(info, PolicyOnRiskCreateClaim(claim), c))
        for datum <- Seq(samplePremiumPaymentInfo(), PolicyClaimPayment) do
            assertRejectedWith("ERR-ON-RISK-13")(
              run(
                datum,
                PolicyOnRiskCreateClaim(claim),
                ctx(datum, Some(withClaim.toData), Seq(policyHolder), afterClaimDate)
              )
            )
    }

    // AcceptClaim

    test("the policy authority accepts a claim") {
        assertSucceeds(
          run(
            withClaim,
            PolicyOnRiskAcceptClaim,
            ctx(withClaim, Some(withAcceptedClaim.toData), Seq(fidaSystem))
          )
        )
        assertSucceeds(
          run(
            withClaim,
            PolicyOnRiskAcceptClaim,
            ctx(withClaim, Some(withAcceptedClaim.toData), Seq(broker1))
          )
        )
    }

    test("a claim is accepted whatever the policy state (Haskell quirk)") {
        for state <- Seq(Funding, Expired) do
            val info = policy(Option.Some(claim), state)
            val c = ctx(info, Some(policy(Option.Some(acceptedClaim), state).toData), Seq(broker1))
            assertSucceeds(run(info, PolicyOnRiskAcceptClaim, c))
    }

    test("ERR-ON-RISK-2: accepting a claim must be signed by the policy authority") {
        for signers <- Seq(Seq.empty, Seq(policyHolder), Seq(fidaSystem, broker1)) do
            val c = ctx(withClaim, Some(withAcceptedClaim.toData), signers)
            assertRejectedWith("ERR-ON-RISK-2")(run(withClaim, PolicyOnRiskAcceptClaim, c))
    }

    test("ERR-ON-RISK-3: the output must hold the accepted claim") {
        for output <- Seq(
              Some(withClaim.toData),
              Some(noClaim.toData),
              Some(policy(Option.Some(acceptedClaim), Funding).toData),
              None
            )
        do
            assertRejectedWith("ERR-ON-RISK-3")(
              run(withClaim, PolicyOnRiskAcceptClaim, ctx(withClaim, output, Seq(broker1)))
            )
    }

    test("ERR-ON-RISK-2 is reported before ERR-ON-RISK-3") {
        assertRejectedWith("ERR-ON-RISK-2")(
          run(withClaim, PolicyOnRiskAcceptClaim, ctx(withClaim, None, Seq(policyHolder)))
        )
    }

    test("ERR-ON-RISK-13: only an existing claim can be accepted") {
        assertRejectedWith("ERR-ON-RISK-13")(
          run(noClaim, PolicyOnRiskAcceptClaim, ctx(noClaim, Some(noClaim.toData), Seq(broker1)))
        )
    }

    // CloseClaim

    test("the policy holder or the policy authority closes a claim") {
        for signer <- Seq(policyHolder, broker1, fidaSystem) do
            assertSucceeds(
              run(
                withClaim,
                PolicyOnRiskCloseClaim,
                ctx(withClaim, Some(noClaim.toData), Seq(signer))
              )
            )
        assertSucceeds(
          run(
            withAcceptedClaim,
            PolicyOnRiskCloseClaim,
            ctx(withAcceptedClaim, Some(noClaim.toData), Seq(policyHolder))
          )
        )
    }

    test(
      "ERR-ON-RISK-4: closing a claim must be signed by the policy holder or the policy authority"
    ) {
        for signers <- Seq(Seq.empty, Seq(investor1), Seq(fidaSystem, broker1)) do
            assertRejectedWith("ERR-ON-RISK-4")(
              run(withClaim, PolicyOnRiskCloseClaim, ctx(withClaim, Some(noClaim.toData), signers))
            )
    }

    test("ERR-ON-RISK-5: closing a claim must unset it") {
        for output <- Seq(Some(withClaim.toData), Some(policy(state = Expired).toData), None) do
            assertRejectedWith("ERR-ON-RISK-5")(
              run(withClaim, PolicyOnRiskCloseClaim, ctx(withClaim, output, Seq(policyHolder)))
            )
    }

    test("ERR-ON-RISK-4 is reported before ERR-ON-RISK-5") {
        assertRejectedWith("ERR-ON-RISK-4")(
          run(withClaim, PolicyOnRiskCloseClaim, ctx(withClaim, None, Seq(investor1)))
        )
    }

    test("ERR-ON-RISK-13: only an existing claim can be closed") {
        assertRejectedWith("ERR-ON-RISK-13")(
          run(
            noClaim,
            PolicyOnRiskCloseClaim,
            ctx(noClaim, Some(noClaim.toData), Seq(policyHolder))
          )
        )
    }

    // FailClaim

    test("the policy authority fails a claim") {
        assertSucceeds(
          run(
            withClaim,
            PolicyOnRiskFailClaim,
            ctx(withClaim, Some(noClaim.toData), Seq(fidaSystem))
          )
        )
    }

    test("FailClaim does not require a claim (Haskell quirk)") {
        assertSucceeds(
          run(noClaim, PolicyOnRiskFailClaim, ctx(noClaim, Some(noClaim.toData), Seq(broker1)))
        )
        val funding = policy(state = Funding)
        assertSucceeds(
          run(funding, PolicyOnRiskFailClaim, ctx(funding, Some(funding.toData), Seq(broker1)))
        )
    }

    test("ERR-ON-RISK-6: failing a claim must be signed by the policy authority") {
        for signers <- Seq(Seq.empty, Seq(policyHolder), Seq(fidaSystem, broker1)) do
            assertRejectedWith("ERR-ON-RISK-6")(
              run(withClaim, PolicyOnRiskFailClaim, ctx(withClaim, Some(noClaim.toData), signers))
            )
    }

    test("ERR-ON-RISK-7: failing a claim must unset it") {
        for output <- Seq(Some(withClaim.toData), Some(policy(state = Cancelled).toData), None) do
            assertRejectedWith("ERR-ON-RISK-7")(
              run(withClaim, PolicyOnRiskFailClaim, ctx(withClaim, output, Seq(broker1)))
            )
    }

    test("ERR-ON-RISK-6 is reported before ERR-ON-RISK-7") {
        assertRejectedWith("ERR-ON-RISK-6")(
          run(withClaim, PolicyOnRiskFailClaim, ctx(withClaim, None, Seq(policyHolder)))
        )
    }

    test("ERR-ON-RISK-13: FailClaim requires an InsuranceInfo datum") {
        for datum <- Seq(samplePremiumPaymentInfo(), PolicyClaimPayment) do
            assertRejectedWith("ERR-ON-RISK-13")(
              run(datum, PolicyOnRiskFailClaim, ctx(datum, Some(noClaim.toData), Seq(broker1)))
            )
    }

    // Cancel

    test("the policy authority cancels an OnRisk policy") {
        val cancelled = policy(Option.Some(claim), Cancelled).toData
        assertSucceeds(
          run(withClaim, PolicyOnRiskCancel, ctx(withClaim, Some(cancelled), Seq(broker1)))
        )
    }

    test("the OnRisk Cancel action cancels a policy in any state but Cancelled (Haskell quirk)") {
        for state <- Seq(Initiated, Funding, Expired) do
            val info = policy(state = state)
            assertSucceeds(
              run(
                info,
                PolicyOnRiskCancel,
                ctx(info, Some(policy(state = Cancelled).toData), Seq(fidaSystem))
              )
            )
    }

    test("ERR-ON-RISK-8: cancelling must be signed by the policy authority") {
        for signers <- Seq(Seq.empty, Seq(policyHolder), Seq(fidaSystem, broker1)) do
            val c = ctx(noClaim, Some(policy(state = Cancelled).toData), signers)
            assertRejectedWith("ERR-ON-RISK-8")(run(noClaim, PolicyOnRiskCancel, c))
    }

    test("ERR-ON-RISK-9: cancelling must set the policy state to Cancelled") {
        for output <- Seq(
              Some(noClaim.toData),
              Some(policy(state = Expired).toData),
              Some(policy(Option.Some(claim), Cancelled).toData),
              None
            )
        do
            assertRejectedWith("ERR-ON-RISK-9")(
              run(noClaim, PolicyOnRiskCancel, ctx(noClaim, output, Seq(broker1)))
            )
    }

    test("ERR-ON-RISK-8 is reported before ERR-ON-RISK-9") {
        assertRejectedWith("ERR-ON-RISK-8")(
          run(noClaim, PolicyOnRiskCancel, ctx(noClaim, None, Seq(investor1)))
        )
    }

    test("ERR-ON-RISK-13: Cancel requires an InsuranceInfo datum") {
        for datum <- Seq(samplePremiumPaymentInfo(), PolicyClaimPayment) do
            assertRejectedWith("ERR-ON-RISK-13")(
              run(datum, PolicyOnRiskCancel, ctx(datum, Some(noClaim.toData), Seq(broker1)))
            )
    }

    // ExpireClaim

    test("anyone expires a claim once its time to live has passed") {
        val after = Interval.after(deadline + 1)
        assertSucceeds(
          run(
            withClaim,
            PolicyOnRiskExpireClaim,
            ctx(withClaim, Some(noClaim.toData), validRange = after)
          )
        )
        assertSucceeds(
          run(
            withAcceptedClaim,
            PolicyOnRiskExpireClaim,
            ctx(withAcceptedClaim, Some(noClaim.toData), Seq(investor1), after)
          )
        )
    }

    test("ERR-ON-RISK-10: expiring a claim must unset it") {
        val after = Interval.after(deadline + 1)
        for output <- Seq(Some(withClaim.toData), Some(policy(state = Expired).toData), None) do
            assertRejectedWith("ERR-ON-RISK-10")(
              run(withClaim, PolicyOnRiskExpireClaim, ctx(withClaim, output, validRange = after))
            )
    }

    test("ERR-ON-RISK-11: a claim expires only after its date plus its time to live") {
        for range <- Seq(
              Interval.after(deadline),
              Interval.after(claim.claimDate + 1),
              Interval.always
            )
        do
            assertRejectedWith("ERR-ON-RISK-11")(
              run(
                withClaim,
                PolicyOnRiskExpireClaim,
                ctx(withClaim, Some(noClaim.toData), validRange = range)
              )
            )
    }

    test("ERR-ON-RISK-10 is reported before ERR-ON-RISK-11") {
        assertRejectedWith("ERR-ON-RISK-10")(
          run(withClaim, PolicyOnRiskExpireClaim, ctx(withClaim, None))
        )
    }

    test("ERR-ON-RISK-13: only an existing claim can expire") {
        val after = Interval.after(deadline + 1)
        assertRejectedWith("ERR-ON-RISK-13")(
          run(
            noClaim,
            PolicyOnRiskExpireClaim,
            ctx(noClaim, Some(noClaim.toData), validRange = after)
          )
        )
    }

    // ClaimPayment

    private val claimPaymentIn =
        txIn(paymentRef, txOut(policyAddress, value(10_000_000), PolicyClaimPayment.toData))

    private def infoReference(
        datum: Data,
        n: Int = 1,
        nft: (ByteString, ByteString, BigInt) = infoNft
    ): TxInInfo =
        txIn(outRef(n), txOut(policyAddress, value(2_000_000, nft), datum))

    private def claimPaymentCtx(
        references: Seq[TxInInfo],
        signers: Seq[PubKeyHash] = Seq(policyHolder)
    ): ScriptContext =
        spending(
          txInfo(
            inputs = Seq(claimPaymentIn),
            referenceInputs = references,
            outputs = Seq(txOutNoDatum(pubKeyAddress(policyHolder), value(10_000_000))),
            signatories = signers
          ),
          paymentRef
        )

    test("the policy holder collects a claim payment, referencing the policy info") {
        assertSucceeds(
          run(
            PolicyClaimPayment,
            PolicyOnRiskClaimPayment,
            claimPaymentCtx(Seq(infoReference(withAcceptedClaim.toData)))
          )
        )
        assertSucceeds(
          run(
            PolicyClaimPayment,
            PolicyOnRiskClaimPayment,
            claimPaymentCtx(Seq(infoReference(noClaim.toData)))
          )
        )
    }

    test("ERR-ON-RISK-12: a claim payment must be signed by the policy holder") {
        for signers <- Seq(Seq.empty, Seq(broker1), Seq(investor1)) do
            assertRejectedWith("ERR-ON-RISK-12")(
              run(
                PolicyClaimPayment,
                PolicyOnRiskClaimPayment,
                claimPaymentCtx(Seq(infoReference(withAcceptedClaim.toData)), signers)
              )
            )
    }

    test("ERR-ON-RISK-12: exactly one reference input must hold the policy info") {
        val info = withAcceptedClaim.toData
        val referenceSets = Seq(
          Seq.empty,
          Seq(infoReference(info, nft = (cs, Tokens.policyInfoTokenName, BigInt(2)))),
          Seq(
            infoReference(
              info,
              nft = (ByteString.fromHex("cd" * 28), Tokens.policyInfoTokenName, BigInt(1))
            )
          ),
          Seq(infoReference(info, nft = (cs, Tokens.policyPaymentTokenName, BigInt(1)))),
          Seq(
            txIn(
              outRef(1),
              TxOut(
                policyAddress,
                value(2_000_000, infoNft),
                OutputDatum.OutputDatumHash(ByteString.fromHex("44" * 32))
              )
            )
          ),
          Seq(infoReference(info), infoReference(info, n = 2))
        )
        for references <- referenceSets do
            assertRejectedWith("ERR-ON-RISK-12")(
              run(PolicyClaimPayment, PolicyOnRiskClaimPayment, claimPaymentCtx(references))
            )
    }

    test(
      "ERR-ON-RISK-12: the policy info is decoded like fromBuiltinData, so malformed ones do not count"
    ) {
        val fields = withAcceptedClaim.toData match
            case Data.Constr(_, args) => args.toScalaList
            case other                => fail(s"unexpected encoding $other")
        def withField(i: Int, d: Data): Data = Data.Constr(0, List.from(fields.updated(i, d)))
        val noFields = Data.Constr(0, List.Nil)
        def claimWith(accepted: Data): Data = Data.Constr(
          0,
          List(
            Data.Constr(
              0,
              List(
                Data.I(1),
                Data.I(2),
                Data.B(ByteString.empty),
                accepted,
                Data.B(ByteString.empty)
              )
            )
          )
        )
        val notConstrs =
            Seq(Data.I(0), Data.B(ByteString.empty), Data.List(List.Nil), Data.Map(List.Nil))
        val structuredFields = Seq(4, 7, 8, 13, 14)
        val notConstrFields =
            for i <- structuredFields; d <- notConstrs yield withField(i, d)
        val notListFields = notConstrs.filterNot(_.isInstanceOf[Data.List]).flatMap { d =>
            Seq(
              withField(4, Data.Constr(0, List(d, Data.I(0)))),
              withField(7, Data.Constr(2, List(d))),
              withField(14, Data.Constr(0, List(d))),
              withField(14, claimWith(d))
            )
        }
        val malformed = notConstrFields ++ notListFields ++ Seq(
          Data.Constr(0, List.from(fields :+ Data.I(0))),
          Data.Constr(0, List.from(fields.init)),
          Data.Constr(3, List.from(fields)),
          withField(0, Data.B(ByteString.empty)),
          withField(4, Data.Constr(0, List(Data.List(List(Data.B(ByteString.empty))), Data.I(0)))),
          withField(4, Data.Constr(0, List(Data.List(List.Nil)))),
          withField(4, Data.Constr(1, List(Data.List(List.Nil), Data.I(0)))),
          withField(5, noFields),
          withField(6, Data.I(4)),
          withField(7, Data.Constr(0, List(Data.List(List(Data.B(policyHolder.hash)))))),
          withField(7, Data.Constr(1, List(Data.B(policyHolder.hash)))),
          withField(7, Data.Constr(1, List(Data.List(List(Data.I(1)))))),
          withField(7, Data.Constr(4, List(Data.List(List.Nil)))),
          withField(8, Data.Constr(5, List.Nil)),
          withField(8, Data.Constr(2, List(Data.I(0)))),
          withField(9, Data.B(ByteString.empty)),
          withField(12, Data.List(List.Nil)),
          withField(13, Data.Constr(0, List.Nil)),
          withField(13, Data.Constr(0, List(Data.B(ByteString.empty)))),
          withField(13, Data.Constr(1, List(Data.I(0)))),
          withField(13, Data.Constr(2, List.Nil)),
          withField(
            14,
            Data.Constr(
              0,
              List(
                Data.Constr(
                  0,
                  List(
                    Data.I(1),
                    Data.I(2),
                    Data.B(ByteString.empty),
                    Data.Constr(2, List.Nil),
                    Data.B(ByteString.empty)
                  )
                )
              )
            )
          ),
          withField(
            14,
            Data.Constr(
              0,
              List(
                Data.Constr(
                  0,
                  List(Data.I(1), Data.I(2), Data.B(ByteString.empty), Data.Constr(1, List.Nil))
                )
              )
            )
          ),
          withField(
            14,
            Data.Constr(
              0,
              List(
                Data.Constr(
                  1,
                  List(
                    Data.I(1),
                    Data.I(2),
                    Data.B(ByteString.empty),
                    Data.Constr(1, List.Nil),
                    Data.B(ByteString.empty)
                  )
                )
              )
            )
          ),
          withField(14, Data.I(0)),
          Data.B(policyHolder.hash),
          samplePremiumPaymentInfo().toData,
          PolicyClaimPayment.toData
        )
        for (datum, i) <- malformed.zipWithIndex do
            withClue(s"malformed policy info #$i: $datum\n") {
                assertRejectedWith("ERR-ON-RISK-12")(
                  run(
                    PolicyClaimPayment,
                    PolicyOnRiskClaimPayment,
                    claimPaymentCtx(Seq(infoReference(datum)))
                  )
                )
            }
    }

    test("well-formed policy infos of every shape count for a claim payment") {
        val authorities = Seq(
          InsuranceAuthority.SingleSign(broker1),
          InsuranceAuthority.AtLeastOneSign(List.Nil),
          InsuranceAuthority.AllMustSign(List(broker1, broker2)),
          InsuranceAuthority.MajorityMustSign(List(fidaSystem))
        )
        for authority <- authorities; state <- InsurancePolicyState.values.toSeq do
            val info = sampleInsuranceInfo(
              state = state,
              claim = Option.Some(acceptedClaim),
              authority = authority
            )
            assertSucceeds(
              run(
                PolicyClaimPayment,
                PolicyOnRiskClaimPayment,
                claimPaymentCtx(Seq(infoReference(info.toData)))
              )
            )
        val noInstallments = noClaim.copy(
          iInfoInstallments = InstallmentsInfo(List.Nil, 0),
          iInfoStartDate = Option.None
        )
        assertSucceeds(
          run(
            PolicyClaimPayment,
            PolicyOnRiskClaimPayment,
            claimPaymentCtx(Seq(infoReference(noInstallments.toData)))
          )
        )
    }

    test("ERR-ON-RISK-12: a policy info that is not signed by its holder does not count") {
        val otherHolder = noClaim.copy(iInfoPolicyHolder = investor1).toData
        assertRejectedWith("ERR-ON-RISK-12")(
          run(
            PolicyClaimPayment,
            PolicyOnRiskClaimPayment,
            claimPaymentCtx(Seq(infoReference(otherHolder)))
          )
        )
        assertSucceeds(
          run(
            PolicyClaimPayment,
            PolicyOnRiskClaimPayment,
            claimPaymentCtx(Seq(infoReference(otherHolder), infoReference(noClaim.toData, n = 2)))
          )
        )
    }

    // Illegal actions

    test("ERR-ON-RISK-13: FinalizeClaim is never allowed") {
        for info <- Seq(noClaim, withClaim, withAcceptedClaim) do
            val signers = Seq(policyHolder, broker1)
            assertRejectedWith("ERR-ON-RISK-13")(
              run(info, PolicyOnRiskFinalizeClaim, ctx(info, Some(noClaim.toData), signers))
            )
        assertRejectedWith("ERR-ON-RISK-13")(
          run(
            PolicyClaimPayment,
            PolicyOnRiskFinalizeClaim,
            claimPaymentCtx(Seq(infoReference(noClaim.toData)))
          )
        )
    }

    test("ERR-ON-RISK-13: ClaimPayment requires a PolicyClaimPayment datum") {
        val references = Seq(infoReference(noClaim.toData, n = 2))
        for datum <- Seq(noClaim, withAcceptedClaim, samplePremiumPaymentInfo()) do
            val c = spending(
              txInfo(
                inputs = Seq(txIn(infoRef, infoOut(datum.toData))),
                referenceInputs = references,
                signatories = Seq(policyHolder)
              ),
              infoRef
            )
            assertRejectedWith("ERR-ON-RISK-13")(run(datum, PolicyOnRiskClaimPayment, c))
    }

    test("ERR-ON-RISK-13: claim actions are rejected for non-InsuranceInfo datums") {
        val redeemers =
            Seq(PolicyOnRiskCloseClaim, PolicyOnRiskAcceptClaim, PolicyOnRiskExpireClaim)
        for datum <- Seq(samplePremiumPaymentInfo(), PolicyClaimPayment); redeemer <- redeemers do
            val c = ctx(
              datum,
              Some(noClaim.toData),
              Seq(policyHolder, broker1),
              Interval.after(deadline + 1)
            )
            assertRejectedWith("ERR-ON-RISK-13")(run(datum, redeemer, c))
    }
}
