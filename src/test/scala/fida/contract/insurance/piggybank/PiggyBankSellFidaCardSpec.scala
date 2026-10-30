package fida.contract.insurance.piggybank

import fida.contract.insurance.PiggyBankDatum.{PBankFidaCard, PBankPremium}
import fida.contract.insurance.PiggyBankRedeemer.SellFidaCard
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.cardano.onchain.plutus.prelude.List

/** `SellFidaCard` on a sold `PBankFidaCard`: ERR-PBANK-5, 6 and 8. */
class PiggyBankSellFidaCardSpec extends PiggyBankSpecBase {

    private val sold = cardDatum(isSold = true)
    private val unsold = cardDatum(isSold = false)
    private val soldValue = value(minAda + fidaCardValue, statusNft)

    /** The card and status NFTs back in the piggy bank, the card value paid out. */
    private val returnedValue = value(minAda, statusNft, cardNft)
    private val payout = txOutNoDatum(investorAddress, value(fidaCardValue))

    private def sell(outputs: TxOut*) =
        spendPiggyBank(Seq(piggyBankInput(sold.toData, soldValue), cardOwnerInput), outputs = outputs)

    test("SellFidaCard: succeeds when the card and status NFTs return with the unsold datum") {
        assertSucceeds(eval(sold, SellFidaCard, sell(piggyBankOutput(unsold.toData, returnedValue), payout)))
    }

    test("quirk: SellFidaCard checks no signature, no payout and only two output datum fields") {
        val rewritten = PBankFidaCard(false, fidaCardId(7), fidaCardValue, List(ByteString.fromString("claim")))
        val drained = value(1, statusNft, cardNft)
        assertSucceeds(eval(sold, SellFidaCard, sell(piggyBankOutput(rewritten.toData, drained))))
    }

    test("ERR-PBANK-5: the returned datum still says the card is sold") {
        assertRejectedWith("ERR-PBANK-5")(
          eval(sold, SellFidaCard, sell(piggyBankOutput(sold.toData, returnedValue), payout))
        )
    }

    test("ERR-PBANK-6: the returned datum changes the card value") {
        val cheaper = cardDatum(isSold = false, cardValue = fidaCardValue - 1)
        assertRejectedWith("ERR-PBANK-6")(
          eval(sold, SellFidaCard, sell(piggyBankOutput(cheaper.toData, returnedValue), payout))
        )
    }

    test("ERR-PBANK-8: no continuing output holds both the card and the status NFT") {
        val statusOnly = piggyBankOutput(unsold.toData, value(minAda, statusNft))
        val cardOnly = piggyBankOutput(unsold.toData, value(minAda, cardNft))
        assertRejectedWith("ERR-PBANK-8")(eval(sold, SellFidaCard, sell(statusOnly, cardOnly, payout)))
    }

    test("ERR-PBANK-8: two continuing outputs hold both NFTs") {
        val returned = piggyBankOutput(unsold.toData, returnedValue)
        assertRejectedWith("ERR-PBANK-8")(eval(sold, SellFidaCard, sell(returned, returned, payout)))
    }

    test("ERR-PBANK-8: the returned datum is a PBankPremium") {
        val premium = PBankPremium(1, 0)
        assertRejectedWith("ERR-PBANK-8")(
          eval(sold, SellFidaCard, sell(piggyBankOutput(premium.toData, returnedValue), payout))
        )
    }

    test("ERR-PBANK-8: a returned datum with an extra field does not decode (exact fromBuiltinData)") {
        val extra = withExtraField(unsold.toData)
        assertRejectedWith("ERR-PBANK-8")(
          eval(sold, SellFidaCard, sell(piggyBankOutput(extra, returnedValue), payout))
        )
    }

    test("ERR-PBANK-8: returned outputs with a non-constructor or malformed datum are skipped, not a crash") {
        val malformed = nonConstructorData ++ Seq(
          withField(unsold.toData, 0, Data.I(0)), // pbfcIsSold
          withField(unsold.toData, 0, constrData(0, Data.I(0))), // a Bool with a field
          withField(unsold.toData, 1, Data.I(1)), // pbfcFidaCardId
          withField(unsold.toData, 3, Data.I(0)), // pbfcPaidClaims
          withField(unsold.toData, 3, listData(Data.I(0))), // a paid claim
          withTag(unsold.toData, 2) // an unknown constructor
        )
        for d <- malformed do
            assertRejectedWith("ERR-PBANK-8")(eval(sold, SellFidaCard, sell(piggyBankOutput(d, returnedValue), payout)))
            assertSucceeds(
              eval(sold, SellFidaCard, sell(piggyBankOutput(d, returnedValue), piggyBankOutput(unsold.toData, returnedValue)))
            )
    }

    test("SellFidaCard: unrelated outputs with non-constructor datums are ignored") {
        val unrelated = nonConstructorData.map(d => txOut(investorAddress, value(minAda), d))
        assertSucceeds(eval(sold, SellFidaCard, sell((piggyBankOutput(unsold.toData, returnedValue) +: unrelated)*)))
    }

    test("ERR-PBANK-8: the returned output has no inline datum") {
        assertRejectedWith("ERR-PBANK-8")(
          eval(sold, SellFidaCard, sell(txOutNoDatum(piggyBankAddress, returnedValue), payout))
        )
    }
}
