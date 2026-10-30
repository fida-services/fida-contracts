package fida.contract.insurance.piggybank

import fida.contract.insurance.InsurancePolicyState.Cancelled
import fida.contract.insurance.PiggyBankRedeemer.{BuyFidaCard, UnlockCollateralOnCancel}
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Decoding as in the Haskell code.
  *
  * The wrapper decodes the parameters, datum and redeemer with PlutusTx `unsafeFromBuiltinData`:
  * strictly, ignoring extra constructor fields, failing with `PT1` on an unknown constructor.
  * Datums the Haskell reads with `fromBuiltinData` (here: policy info reference inputs) count only
  * when they decode exactly.
  */
class PiggyBankDecodingSpec extends PiggyBankSpecBase {

    private val sold = cardDatum(isSold = true)
    private val unsold = cardDatum(isSold = false)
    private val cancelled = sampleInsuranceInfo(state = Cancelled)
    private val collateral = value(minAda + fidaCardValue, statusNft)

    /** A transaction that `UnlockCollateralOnCancel` accepts for any well-formed card datum. */
    private def unlockOnCancel(datum: Data, refs: Seq[Data] = Seq(cancelled.toData)) =
        spendPiggyBank(
          Seq(piggyBankInput(datum, collateral), cardOwnerInput),
          refs = refs.zipWithIndex.map((d, i) => policyInfoRef(d, i + 1)),
          outputs = Seq(txOutNoDatum(investorAddress, collateral))
        )

    private def unlockOnCancelWith(datum: Data, redeemer: Data = UnlockCollateralOnCancel.toData) =
        evalRaw(datum, redeemer, unlockOnCancel(datum))

    test("the reference transaction is accepted with a well-formed datum") {
        assertSucceeds(unlockOnCancelWith(sold.toData))
    }

    test("a datum with an unknown constructor fails with PT1") {
        assertFailsWithoutErrorCode(unlockOnCancelWith(withTag(sold.toData, 2)), Some("PT1"))
    }

    test("a redeemer with an unknown constructor fails with PT1") {
        val ctx = unlockOnCancel(sold.toData)
        assertFailsWithoutErrorCode(evalRaw(sold.toData, constrData(7), ctx), Some("PT1"))
        assertFailsWithoutErrorCode(evalRaw(sold.toData, Data.I(5), ctx))
    }

    test("redeemer constructor fields are ignored") {
        assertSucceeds(unlockOnCancelWith(sold.toData, constrData(5, Data.I(1))))
    }

    test("datum fields are decoded strictly, also those the action does not use") {
        val malformed = Seq(
          withField(sold.toData, 0, Data.I(1)), // pbfcIsSold is not a Bool
          withField(sold.toData, 1, Data.I(1)), // pbfcFidaCardId is not a bytestring
          withField(sold.toData, 2, Data.B(ByteString.fromString("1"))), // pbfcFidaCardValue
          withField(sold.toData, 3, listData(Data.I(1))), // a paid claim is not a bytestring
          constrData(1, constrData(1), Data.B(fcid.id), Data.I(fidaCardValue)) // a missing field
        )
        for d <- malformed do assertFailsWithoutErrorCode(unlockOnCancelWith(d))
    }

    test("a Bool with an unknown constructor in the datum fails with PT1") {
        assertFailsWithoutErrorCode(
          unlockOnCancelWith(withField(sold.toData, 0, constrData(2))),
          Some("PT1")
        )
    }

    test("extra constructor fields in the datum are ignored") {
        assertSucceeds(unlockOnCancelWith(withExtraField(sold.toData)))
        assertSucceeds(unlockOnCancelWith(withField(sold.toData, 0, constrData(1, Data.I(7)))))
    }

    test("BuyFidaCard compares the output with the canonical re-encoding of the decoded datum") {
        val input = withExtraField(unsold.toData)
        def buy(outputDatum: Data) = spendPiggyBank(
          Seq(piggyBankInput(input, value(minAda, statusNft, cardNft)), walletInput),
          outputs = Seq(piggyBankOutput(outputDatum, value(minAda + fidaCardValue, statusNft)))
        )
        assertSucceeds(evalRaw(input, BuyFidaCard.toData, buy(sold.toData)))
        assertRejectedWith("ERR-PBANK-0")(
          evalRaw(input, BuyFidaCard.toData, buy(withExtraField(sold.toData)))
        )
    }

    test("parameters that are not bytestrings fail every action, even where they are unused") {
        val buy = spendPiggyBank(
          Seq(piggyBankInput(unsold.toData, value(minAda, statusNft, cardNft)), walletInput),
          outputs = Seq(piggyBankOutput(sold.toData, value(minAda + fidaCardValue, statusNft)))
        )
        assertSucceeds(evalRaw(unsold.toData, BuyFidaCard.toData, buy))
        assertFailsWithoutErrorCode(
          evalRaw(unsold.toData, BuyFidaCard.toData, buy, card = Data.I(1))
        )
        assertFailsWithoutErrorCode(
          evalRaw(unsold.toData, BuyFidaCard.toData, buy, insurance = constrData(0, Data.B(cs)))
        )
    }

    test("policy info datums that do not decode exactly (fromBuiltinData) are ignored") {
        val withClaim = sampleInsuranceInfo(
          state = Cancelled,
          claim = scalus.cardano.onchain.plutus.prelude.Option.Some(sampleClaim)
        )
        val claimWithBadBool =
            constrData(0, withField(sampleClaim.toData, 3, constrData(0, Data.I(0))))
        val undecodable = Seq(
          withExtraField(cancelled.toData),
          withField(
            cancelled.toData,
            7,
            constrData(4, Data.B(cs))
          ), // unknown authority constructor
          withField(cancelled.toData, 8, constrData(3, Data.I(0))), // a state with a field
          withField(
            cancelled.toData,
            4,
            constrData(0, listData(Data.I(1)), Data.I(1), Data.I(1))
          ), // installments
          withField(cancelled.toData, 6, Data.I(4)), // policy holder is not a bytestring
          withField(withClaim.toData, 14, claimWithBadBool) // claimAccepted: a Bool with a field
        )
        for d <- undecodable do
            assertRejectedWith("ERR-PBANK-27.1")(
              evalRaw(
                sold.toData,
                UnlockCollateralOnCancel.toData,
                unlockOnCancel(sold.toData, Seq(d))
              )
            )
        // a well-formed claim decodes, and one undecodable datum next to a valid one is skipped
        assertSucceeds(
          evalRaw(
            sold.toData,
            UnlockCollateralOnCancel.toData,
            unlockOnCancel(sold.toData, Seq(withClaim.toData))
          )
        )
        assertSucceeds(
          evalRaw(
            sold.toData,
            UnlockCollateralOnCancel.toData,
            unlockOnCancel(sold.toData, Seq(cancelled.toData, undecodable.head))
          )
        )
    }

    test(
      "policy info reference inputs with a non-constructor inline datum are skipped, not a crash"
    ) {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-27.1")(
              evalRaw(
                sold.toData,
                UnlockCollateralOnCancel.toData,
                unlockOnCancel(sold.toData, Seq(d))
              )
            )
            assertSucceeds(
              evalRaw(
                sold.toData,
                UnlockCollateralOnCancel.toData,
                unlockOnCancel(sold.toData, Seq(d, cancelled.toData))
              )
            )
    }

    test(
      "policy info datums with non-constructor data where a constructor is expected are skipped"
    ) {
        val withClaim = sampleInsuranceInfo(
          state = Cancelled,
          claim = scalus.cardano.onchain.plutus.prelude.Option.Some(sampleClaim)
        ).toData
        val i = Data.I(5)
        val b = Data.B(ByteString.fromString("x"))
        val nested = Seq(
          withField(cancelled.toData, 4, i), // iInfoInstallments
          withField(cancelled.toData, 4, constrData(0, i, Data.I(1))), // payment intervals
          withField(cancelled.toData, 4, constrData(0, listData(i), b)), // amount per installment
          withField(cancelled.toData, 7, b), // iInfoPolicyAuthority
          withField(cancelled.toData, 7, constrData(1, i)), // AtLeastOneSign without a list
          withField(cancelled.toData, 7, constrData(2, listData(i))), // AllMustSign of a non-hash
          withField(cancelled.toData, 7, constrData(0, i)), // SingleSign of a non-hash
          withField(cancelled.toData, 8, Data.I(3)), // iInfoState
          withField(cancelled.toData, 13, i), // iInfoStartDate
          withField(cancelled.toData, 13, constrData(0, b)), // Just a non-time
          withField(cancelled.toData, 14, i), // iInfoClaim
          withField(cancelled.toData, 14, constrData(0, i)), // Just a non-ClaimInfo
          withField(
            withClaim,
            14,
            constrData(0, withField(sampleClaim.toData, 3, Data.I(1)))
          ) // claimAccepted
        )
        for d <- nested do
            assertRejectedWith("ERR-PBANK-27.1")(
              evalRaw(
                sold.toData,
                UnlockCollateralOnCancel.toData,
                unlockOnCancel(sold.toData, Seq(d))
              )
            )
    }

    test("a policy info reference input without inline datum is ignored") {
        val noDatum = txIn(outRef(1), txOutNoDatum(policyAddress, value(minAda, infoNft)))
        val ctx = spendPiggyBank(
          Seq(piggyBankInput(sold.toData, collateral), cardOwnerInput),
          refs = Seq(noDatum)
        )
        assertRejectedWith("ERR-PBANK-27.1")(eval(sold, UnlockCollateralOnCancel, ctx))
    }

    test("a reference input holding two policy info tokens is ignored") {
        val twoTokens = txIn(
          outRef(1),
          txOut(policyAddress, value(minAda, (cs, infoNft._2, BigInt(2))), cancelled.toData)
        )
        val ctx = spendPiggyBank(
          Seq(piggyBankInput(sold.toData, collateral), cardOwnerInput),
          refs = Seq(twoTokens)
        )
        assertRejectedWith("ERR-PBANK-27.1")(eval(sold, UnlockCollateralOnCancel, ctx))
    }
}
