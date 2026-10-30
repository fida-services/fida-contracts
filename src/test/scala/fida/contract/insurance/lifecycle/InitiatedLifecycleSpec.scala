package fida.contract.insurance.lifecycle

import fida.contract.Scripts
import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PolicyClaimPayment}
import fida.contract.insurance.InsurancePolicyRedeemer.PolicyInitiated
import fida.contract.insurance.InsurancePolicyState.*
import fida.contract.insurance.PiggyBankDatum.{PBankFidaCard, PBankPremium}
import fida.contract.insurance.PolicyInitiatedRedeemer.{
    PolicyInitiatedCancel,
    PolicyInitiatedPayPremium
}
import fida.contract.testkit.{ScriptAssertions, ScriptEval}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.Result
import scalus.cardano.onchain.plutus.v1.{
    Address,
    Credential,
    PubKeyHash,
    StakingCredential,
    TxOutRef
}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, ScriptContext, TxInInfo, TxOut}
import scalus.cardano.onchain.plutus.prelude.{List, Option}

/** Direct-evaluation tests of the Initiated lifecycle (port of
  * `Fida.Contract.Insurance.Lifecycle.Initiated`), run through the full insurance policy validator.
  */
class InitiatedLifecycleSpec extends AnyFunSuite with ScriptAssertions {

    private val cs = insuranceId.cs
    private val policyAddress = scriptAddress(scriptHash(1))
    private val infoNft = (cs, Tokens.policyInfoTokenName, BigInt(1))
    private val paymentNft = (cs, Tokens.policyPaymentTokenName, BigInt(1))

    private val infoRef = outRef(1)
    private val paymentRef = outRef(2)

    private val initiated: InsuranceInfo = sampleInsuranceInfo()
    private val premiumInfo = samplePremiumPaymentInfo()
    private val perPiggyBank: BigInt = premiumInfo.ppInfoPremiumAmountPerPiggyBank
    private val piggyBanks: Seq[Address] = premiumInfo.ppInfoPiggyBanks.toScalaList

    private val cancel = PolicyInitiated(PolicyInitiatedCancel)
    private val payPremium = PolicyInitiated(PolicyInitiatedPayPremium)

    private def run(
        datum: InsurancePolicyDatum,
        redeemer: InsurancePolicyRedeemer,
        ctx: ScriptContext
    ): Result =
        ScriptEval.eval(
          Scripts.insurancePolicyValidator.program,
          insuranceId.toData,
          datum.toData,
          redeemer.toData,
          ctx.toData
        )

    /** Rejected, and `code` is the only error code in the logs: the one the Haskell validator
      * reports.
      */
    private def assertRejectedWith(code: String)(result: Result): Unit =
        assertFailsWith(code)(result)
        val codes = result.logs.filter(_.startsWith("ERR-"))
        assert(codes == Seq(code), s"expected only $code, got ${codes.mkString("[", ", ", "]")}")

    private def withState(info: InsuranceInfo, state: InsurancePolicyState): Data =
        info.copy(iInfoState = state).toData

    private def infoOut(datum: Data): TxOut = txOut(policyAddress, value(2_000_000, infoNft), datum)
    private def infoIn(datum: InsurancePolicyDatum): TxInInfo = txIn(infoRef, infoOut(datum.toData))

    private val paymentIn =
        txIn(paymentRef, txOut(policyAddress, value(2_000_000, paymentNft), premiumInfo.toData))
    private val holderFunds =
        txIn(outRef(3), txOutNoDatum(pubKeyAddress(policyHolder), value(500_000_000)))
    private val paymentInfoToHolder =
        txOutNoDatum(pubKeyAddress(policyHolder), value(2_000_000, paymentNft))

    private def premiumPayment(
        to: Address,
        paid: BigInt = perPiggyBank,
        amount: BigInt = perPiggyBank,
        refund: BigInt = 0
    ): TxOut = txOut(to, value(paid), PBankPremium(amount, refund).toData)

    private val allPayments: Seq[TxOut] = piggyBanks.map(premiumPayment(_))

    /** Spends the Initiated policy info and recreates it with `output`. */
    private def cancelCtx(
        output: Seq[TxOut] = Seq(infoOut(withState(initiated, Cancelled))),
        signers: Seq[PubKeyHash] = Seq(broker1),
        info: InsurancePolicyDatum = initiated
    ): ScriptContext =
        spending(
          txInfo(inputs = Seq(infoIn(info)), outputs = output, signatories = signers),
          infoRef
        )

    /** The Haskell toolbox `payPremium` transaction: spends the policy info and the premium payment
      * info, moves the policy to Funding, pays every piggy bank and hands the payment info value to
      * the policy holder. `spent` selects the input being validated.
      */
    private def payPremiumCtx(
        spent: TxOutRef,
        info: InsurancePolicyDatum = initiated,
        infoOutput: Seq[TxOut] = Seq(infoOut(withState(initiated, Funding))),
        payments: Seq[TxOut] = allPayments,
        otherInputs: Seq[TxInInfo] = Seq(paymentIn, holderFunds),
        extraOutputs: Seq[TxOut] = Seq.empty
    ): ScriptContext =
        spending(
          txInfo(
            inputs = infoIn(info) +: otherInputs,
            outputs = infoOutput ++ payments ++ Seq(paymentInfoToHolder) ++ extraOutputs,
            signatories = Seq(policyHolder)
          ),
          spent
        )

    /** `allPayments` with the payment to the first piggy bank replaced by `replacement`. */
    private def firstPaymentReplacedBy(replacement: TxOut*): Seq[TxOut] =
        replacement ++ allPayments.tail

    // Cancel

    test("the policy authority cancels an Initiated policy") {
        assertSucceeds(run(initiated, cancel, cancelCtx()))
        assertSucceeds(run(initiated, cancel, cancelCtx(signers = Seq(fidaSystem, investor1))))
    }

    test("ERR-INIT-1: cancelling requires the policy authority's signature") {
        for signers <- Seq(Seq.empty, Seq(policyHolder), Seq(investor1, broker2)) do
            assertRejectedWith("ERR-INIT-1")(run(initiated, cancel, cancelCtx(signers = signers)))
    }

    test(
      "ERR-INIT-1: AtLeastOneSign is not satisfied when two authority keys sign (Haskell quirk)"
    ) {
        assertRejectedWith("ERR-INIT-1")(
          run(initiated, cancel, cancelCtx(signers = Seq(fidaSystem, broker1)))
        )
    }

    test("ERR-INIT-1 is reported before ERR-INIT-2") {
        val ctx = cancelCtx(
          output = Seq(infoOut(withState(initiated, Funding))),
          signers = Seq(investor1)
        )
        assertRejectedWith("ERR-INIT-1")(run(initiated, cancel, ctx))
    }

    test("ERR-INIT-2: cancelling must set the policy state to Cancelled") {
        for state <- Seq(Initiated, Funding, OnRisk, Expired) do
            assertRejectedWith("ERR-INIT-2")(
              run(initiated, cancel, cancelCtx(output = Seq(infoOut(withState(initiated, state)))))
            )
        val otherChange = initiated.copy(iInfoState = Cancelled, iInfoFidaCardNumber = 11).toData
        assertRejectedWith("ERR-INIT-2")(
          run(initiated, cancel, cancelCtx(output = Seq(infoOut(otherChange))))
        )
    }

    test(
      "ERR-INIT-2: the cancelled policy info must be the single continuing output with the NFT and an inline datum"
    ) {
        val cancelled = withState(initiated, Cancelled)
        val outputs = Seq(
          Seq.empty,
          Seq(txOut(policyAddress, value(2_000_000), cancelled)),
          Seq(
            txOut(
              policyAddress,
              value(2_000_000, (cs, Tokens.policyInfoTokenName, BigInt(2))),
              cancelled
            )
          ),
          Seq(txOutNoDatum(policyAddress, value(2_000_000, infoNft))),
          Seq(
            TxOut(
              policyAddress,
              value(2_000_000, infoNft),
              OutputDatum.OutputDatumHash(ByteString.fromHex("11" * 32))
            )
          ),
          Seq(txOut(pubKeyAddress(broker1), value(2_000_000, infoNft), cancelled)),
          Seq(infoOut(cancelled), infoOut(cancelled))
        )
        for output <- outputs do
            assertRejectedWith("ERR-INIT-2")(run(initiated, cancel, cancelCtx(output = output)))
    }

    test("ERR-INIT-0: Cancel is only allowed for an InsuranceInfo in state Initiated") {
        for state <- Seq(Funding, OnRisk, Cancelled, Expired) do
            val info = initiated.copy(iInfoState = state)
            assertRejectedWith("ERR-INIT-0")(run(info, cancel, cancelCtx(info = info)))
        assertRejectedWith("ERR-INIT-0")(
          run(
            premiumInfo,
            cancel,
            spending(txInfo(inputs = Seq(paymentIn), signatories = Seq(broker1)), paymentRef)
          )
        )
        assertRejectedWith("ERR-INIT-0")(
          run(PolicyClaimPayment, cancel, cancelCtx(info = PolicyClaimPayment))
        )
    }

    test("ERR-INIT-0: PayPremium is not allowed for a PolicyClaimPayment datum") {
        assertRejectedWith("ERR-INIT-0")(
          run(PolicyClaimPayment, payPremium, payPremiumCtx(infoRef, info = PolicyClaimPayment))
        )
    }

    // PayPremium, policy info input

    test("paying the premium moves the policy info to Funding") {
        assertSucceeds(run(initiated, payPremium, payPremiumCtx(infoRef)))
    }

    test("ERR-INIT-6: paying the premium must set the policy state to Funding") {
        for state <- Seq(Initiated, OnRisk, Cancelled, Expired) do
            val ctx = payPremiumCtx(infoRef, infoOutput = Seq(infoOut(withState(initiated, state))))
            assertRejectedWith("ERR-INIT-6")(run(initiated, payPremium, ctx))
        assertRejectedWith("ERR-INIT-6")(
          run(initiated, payPremium, payPremiumCtx(infoRef, infoOutput = Seq.empty))
        )
    }

    test("ERR-INIT-7: exactly one input must hold exactly one POLICY-PAYMENT token") {
        val twoTokens = txIn(
          paymentRef,
          txOut(
            policyAddress,
            value(2_000_000, (cs, Tokens.policyPaymentTokenName, BigInt(2))),
            premiumInfo.toData
          )
        )
        val walletToken =
            txIn(outRef(4), txOutNoDatum(pubKeyAddress(policyHolder), value(2_000_000, paymentNft)))
        val otherPolicyToken = txIn(
          paymentRef,
          txOut(
            policyAddress,
            value(
              2_000_000,
              (ByteString.fromHex("cd" * 28), Tokens.policyPaymentTokenName, BigInt(1))
            ),
            premiumInfo.toData
          )
        )
        for otherInputs <- Seq(
              Seq(holderFunds),
              Seq(twoTokens, holderFunds),
              Seq(otherPolicyToken),
              Seq(paymentIn, walletToken)
            )
        do
            assertRejectedWith("ERR-INIT-7")(
              run(initiated, payPremium, payPremiumCtx(infoRef, otherInputs = otherInputs))
            )
    }

    test("ERR-INIT-6 is reported before ERR-INIT-7") {
        val ctx = payPremiumCtx(infoRef, infoOutput = Seq.empty, otherInputs = Seq(holderFunds))
        assertRejectedWith("ERR-INIT-6")(run(initiated, payPremium, ctx))
    }

    test(
      "a policy in any state returns to Funding when a POLICY-PAYMENT token is spent (Haskell quirk)"
    ) {
        val walletToken =
            txIn(outRef(4), txOutNoDatum(pubKeyAddress(policyHolder), value(2_000_000, paymentNft)))
        for state <- Seq(Funding, OnRisk, Cancelled, Expired) do
            val info =
                initiated.copy(iInfoState = state, iInfoStartDate = Option.Some(beginningOfTime))
            val ctx = payPremiumCtx(
              infoRef,
              info = info,
              infoOutput = Seq(infoOut(withState(info, Funding))),
              payments = Seq.empty,
              otherInputs = Seq(walletToken)
            )
            assertSucceeds(run(info, payPremium, ctx))
    }

    // PayPremium, premium payment info input

    test("the premium payment info is spent when every piggy bank receives its premium") {
        assertSucceeds(run(premiumInfo, payPremium, payPremiumCtx(paymentRef)))
    }

    test("overpayments, duplicate payments and unrelated outputs are accepted") {
        val stakedPiggyBank = Address(
          piggyBanks.head.credential,
          Option.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(pkh(9))))
        )
        val payments = firstPaymentReplacedBy(
          premiumPayment(piggyBanks.head, paid = perPiggyBank + 5, amount = perPiggyBank + 5)
        )
        val extra = Seq(
          premiumPayment(piggyBanks(1)),
          premiumPayment(piggyBanks(2), paid = 1, amount = 1),
          premiumPayment(stakedPiggyBank),
          premiumPayment(pubKeyAddress(investor1)),
          txOut(piggyBanks(3), value(perPiggyBank), Data.I(42)),
          txOut(piggyBanks(3), value(perPiggyBank), Data.B(ByteString.fromHex("abcd"))),
          txOut(
            piggyBanks(3),
            value(perPiggyBank),
            Data.List(List(Data.I(perPiggyBank), Data.I(0)))
          ),
          txOut(
            piggyBanks(3),
            value(perPiggyBank),
            Data.Map(List((Data.I(perPiggyBank), Data.I(0))))
          ),
          txOut(
            piggyBanks(3),
            value(perPiggyBank),
            Data.Constr(0, List(Data.B(ByteString.empty), Data.I(0)))
          ),
          txOut(
            piggyBanks(3),
            value(perPiggyBank),
            PBankFidaCard(true, fidaCardId(1), fidaCardValue, List.Nil).toData
          )
        )
        assertSucceeds(
          run(
            premiumInfo,
            payPremium,
            payPremiumCtx(paymentRef, payments = payments, extraOutputs = extra)
          )
        )
    }

    test("ERR-INIT-3: every piggy bank must receive a valid premium payment") {
        val first = piggyBanks.head
        val stakedFirst = Address(
          first.credential,
          Option.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(pkh(9))))
        )
        val brokenPayments = Seq(
          Seq.empty,
          Seq(premiumPayment(first, paid = perPiggyBank - 1, amount = perPiggyBank - 1)),
          Seq(premiumPayment(first, paid = perPiggyBank, amount = perPiggyBank + 1)),
          Seq(premiumPayment(first, paid = perPiggyBank + 1, amount = perPiggyBank)),
          Seq(premiumPayment(first, refund = 1)),
          Seq(premiumPayment(stakedFirst)),
          Seq(txOutNoDatum(first, value(perPiggyBank))),
          Seq(
            TxOut(
              first,
              value(perPiggyBank),
              OutputDatum.OutputDatumHash(ByteString.fromHex("22" * 32))
            )
          )
        )
        for payment <- brokenPayments do
            val ctx = payPremiumCtx(paymentRef, payments = firstPaymentReplacedBy(payment*))
            assertRejectedWith("ERR-INIT-3")(run(premiumInfo, payPremium, ctx))
    }

    test(
      "ERR-INIT-3: payment datums are decoded like fromBuiltinData, so malformed ones are not payments"
    ) {
        val first = piggyBanks.head
        val amount = Data.I(perPiggyBank)
        val malformedDatums = Seq(
          Data.Constr(0, List(amount, Data.I(0), Data.I(0))),
          Data.Constr(0, List(amount)),
          Data.Constr(0, List(amount, Data.B(ByteString.empty))),
          Data.Constr(0, List(Data.B(ByteString.empty), Data.I(0))),
          Data.Constr(2, List(amount, Data.I(0))),
          Data.Constr(0, List(amount, Data.List(List.Nil))),
          Data.Constr(0, List(Data.Map(List.Nil), Data.I(0))),
          Data.I(perPiggyBank),
          Data.B(ByteString.empty),
          Data.List(List(amount, Data.I(0))),
          Data.Map(List((amount, Data.I(0)))),
          PBankFidaCard(true, fidaCardId(1), perPiggyBank, List.Nil).toData
        )
        for (datum, i) <- malformedDatums.zipWithIndex do
            withClue(s"malformed payment #$i: $datum\n") {
                val ctx = payPremiumCtx(
                  paymentRef,
                  payments = firstPaymentReplacedBy(txOut(first, value(perPiggyBank), datum))
                )
                assertRejectedWith("ERR-INIT-3")(run(premiumInfo, payPremium, ctx))
            }
    }

    test(
      "ERR-INIT-3: a premium payment info listing a piggy bank twice can never be paid (Haskell quirk)"
    ) {
        val duplicated = premiumInfo.copy(ppInfoPiggyBanks =
            List.Cons(piggyBanks.head, premiumInfo.ppInfoPiggyBanks)
        )
        val in =
            txIn(paymentRef, txOut(policyAddress, value(2_000_000, paymentNft), duplicated.toData))
        val ctx = payPremiumCtx(paymentRef, otherInputs = Seq(in, holderFunds))
        assertRejectedWith("ERR-INIT-3")(run(duplicated, payPremium, ctx))
    }

    test("ERR-INIT-4: the policy info must be recreated by exactly one continuing output") {
        val funding = withState(initiated, Funding)
        val infoOutputs = Seq(
          Seq.empty,
          Seq(txOut(pubKeyAddress(policyHolder), value(2_000_000, infoNft), funding)),
          Seq(
            txOut(
              policyAddress,
              value(2_000_000, (cs, Tokens.policyInfoTokenName, BigInt(2))),
              funding
            )
          ),
          Seq(infoOut(funding), infoOut(funding))
        )
        for infoOutput <- infoOutputs do
            assertRejectedWith("ERR-INIT-4")(
              run(premiumInfo, payPremium, payPremiumCtx(paymentRef, infoOutput = infoOutput))
            )
    }

    test(
      "ERR-INIT-4 only looks at the POLICY-INFO token, not at the datum of the recreated policy info"
    ) {
        for infoOutput <- Seq(
              infoOut(withState(initiated, Initiated)),
              txOutNoDatum(policyAddress, value(2_000_000, infoNft))
            )
        do
            assertSucceeds(
              run(premiumInfo, payPremium, payPremiumCtx(paymentRef, infoOutput = Seq(infoOutput)))
            )
    }

    test("ERR-INIT-3 is reported before ERR-INIT-4") {
        val ctx = payPremiumCtx(paymentRef, infoOutput = Seq.empty, payments = allPayments.tail)
        assertRejectedWith("ERR-INIT-3")(run(premiumInfo, payPremium, ctx))
    }
}
