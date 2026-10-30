package fida.contract.insurance.piggybank

import fida.contract.insurance.PiggyBankRedeemer.BuyFidaCard
import fida.contract.testkit.ContextBuilder.*
import fida.contract.testkit.Fixtures.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v2.TxOut

/** `BuyFidaCard` on an unsold `PBankFidaCard`: ERR-PBANK-0, 3 and 4 (ERR-PBANK-2 is unreachable).
  */
class PiggyBankBuyFidaCardSpec extends PiggyBankSpecBase {

    private val unsold = cardDatum(isSold = false)
    private val sold = cardDatum(isSold = true)

    /** A piggy bank as created with the policy: min ADA, the status NFT and the card NFT. */
    private val freshValue = value(minAda, statusNft, cardNft)

    /** The piggy bank after the sale: the card value on top of the input lovelace. */
    private val paidValue = value(minAda + fidaCardValue, statusNft)

    private val cardToBuyer = txOutNoDatum(investorAddress, value(minAda, cardNft))

    private def buy(outputs: TxOut*) =
        spendPiggyBank(
          Seq(piggyBankInput(unsold.toData, freshValue), walletInput),
          outputs = outputs
        )

    test("BuyFidaCard: succeeds when the status output holds the card value and the sold datum") {
        assertSucceeds(
          eval(unsold, BuyFidaCard, buy(piggyBankOutput(sold.toData, paidValue), cardToBuyer))
        )
    }

    test("BuyFidaCard: no signature is needed and the card NFT may go anywhere") {
        val keepsCard = value(minAda + fidaCardValue, statusNft, cardNft)
        assertSucceeds(eval(unsold, BuyFidaCard, buy(piggyBankOutput(sold.toData, keepsCard))))
    }

    test("ERR-PBANK-0: the output datum still says the card is unsold") {
        assertRejectedWith("ERR-PBANK-0")(
          eval(unsold, BuyFidaCard, buy(piggyBankOutput(unsold.toData, paidValue), cardToBuyer))
        )
    }

    test("ERR-PBANK-0: the output datum may differ from the input datum only in pbfcIsSold") {
        val changed = cardDatum(isSold = true, paidClaims = Seq(ByteString.fromString("claim")))
        assertRejectedWith("ERR-PBANK-0")(
          eval(unsold, BuyFidaCard, buy(piggyBankOutput(changed.toData, paidValue), cardToBuyer))
        )
    }

    test("ERR-PBANK-0: a status output with a non-constructor datum (compared as BuiltinData)") {
        for d <- nonConstructorData do
            assertRejectedWith("ERR-PBANK-0")(
              eval(unsold, BuyFidaCard, buy(piggyBankOutput(d, paidValue), cardToBuyer))
            )
    }

    test("BuyFidaCard: other outputs with non-constructor datums are ignored") {
        val others = nonConstructorData.flatMap(d =>
            Seq(txOut(investorAddress, value(minAda), d), piggyBankOutput(d, value(minAda)))
        )
        assertSucceeds(
          eval(unsold, BuyFidaCard, buy((piggyBankOutput(sold.toData, paidValue) +: others)*))
        )
    }

    test("ERR-PBANK-3: no continuing output holds the status NFT") {
        val elsewhere = txOut(investorAddress, paidValue, sold.toData)
        assertRejectedWith("ERR-PBANK-3")(eval(unsold, BuyFidaCard, buy(elsewhere, cardToBuyer)))
    }

    test("ERR-PBANK-3: two continuing outputs hold a status NFT") {
        val second = piggyBankOutput(sold.toData, value(minAda, statusNft))
        assertRejectedWith("ERR-PBANK-3")(
          eval(unsold, BuyFidaCard, buy(piggyBankOutput(sold.toData, paidValue), second))
        )
    }

    test(
      "ERR-PBANK-4: the status output holds less than the card value on top of the input lovelace"
    ) {
        val short = value(minAda + fidaCardValue - 1, statusNft)
        assertRejectedWith("ERR-PBANK-4")(
          eval(unsold, BuyFidaCard, buy(piggyBankOutput(sold.toData, short), cardToBuyer))
        )
    }

    test("ERR-PBANK-4 is checked before the datum: a short output with the wrong datum") {
        val short = value(minAda + fidaCardValue - 1, statusNft)
        assertRejectedWith("ERR-PBANK-4")(
          eval(unsold, BuyFidaCard, buy(piggyBankOutput(unsold.toData, short), cardToBuyer))
        )
    }

    test(
      "BuyFidaCard: a status output without inline datum fails without code (incomplete Haskell case)"
    ) {
        val noDatum = txOutNoDatum(piggyBankAddress, paidValue)
        assertFailsWithoutErrorCode(eval(unsold, BuyFidaCard, buy(noDatum, cardToBuyer)))
    }

    test("ERR-PBANK-2 is unreachable: without own input getContinuingOutputs fails first with Lf") {
        val info = txInfo(
          inputs = Seq(piggyBankInput(unsold.toData, freshValue), walletInput),
          outputs = Seq(piggyBankOutput(sold.toData, paidValue))
        )
        assertFailsWithoutErrorCode(eval(unsold, BuyFidaCard, minting(info, cs)), Some("Lf"))
        assertFailsWithoutErrorCode(
          eval(unsold, BuyFidaCard, spending(info, outRef(999))),
          Some("Lf")
        )
    }
}
