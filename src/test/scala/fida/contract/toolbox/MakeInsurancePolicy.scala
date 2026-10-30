package fida.contract.toolbox

import fida.contract.Scripts
import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PremiumPaymentInfo}
import fida.contract.insurance.PiggyBankDatum.PBankFidaCard
import fida.contract.toolbox.Validators.*
import scalus.cardano.ledger.{AssetName, Script, ScriptRef, TransactionOutput, Value}
import scalus.cardano.onchain.plutus.v1.{PosixTime, TxId, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}

/** Policy creation (port of `Fida.Contract.TestToolbox.Action.MakeInsurancePolicy`). */
object MakeInsurancePolicy {

    final case class InsuranceCreateParams(
        icpPolicyHolder: User,
        icpPolicyPrice: BigInt,
        icpFidaCardQuantity: Int,
        icpFidaCardValue: BigInt,
        icpPolicyAuthority: InsuranceAuthority,
        icpFundingDeadline: PosixTime
    )

    def iinfoFromParams(params: InsuranceCreateParams): InsuranceInfo =
        import params.*
        InsuranceInfo(
          iInfoCollateralAmount = icpFidaCardQuantity * icpFidaCardValue,
          iInfoFidaCardValue = icpFidaCardValue,
          iInfoFidaCardNumber = icpFidaCardQuantity,
          iInfoPremiumAmount = icpPolicyPrice,
          iInfoInstallments = InstallmentsInfo(
            PList.from(Seq(Time.days(90), Time.days(90), Time.days(90))),
            icpPolicyPrice / icpFidaCardQuantity / 4
          ),
          iInfoInsurancePeriod = Time.days(365),
          iInfoPolicyHolder = icpPolicyHolder.pkh,
          iInfoPolicyAuthority = icpPolicyAuthority,
          iInfoState = InsurancePolicyState.Initiated,
          iInfoFundingDeadline = icpFundingDeadline,
          iInfoTotalClaimsAcceptedAmount = 0,
          iInfoClaimTimeToLive = Time.seconds(100),
          iInfoClaimTimeToPay = Time.seconds(50),
          iInfoStartDate = POption.None,
          iInfoClaim = POption.None
        )

    /** The sample policy: 10 fida cards of 1 000 ADA, a 200 ADA premium, authority
      * `AtLeastOneSign [fidaSystem, broker1]`, funding deadline in 7 days, created by broker 1.
      */
    def newSamplePolicy(users: Users)(using run: Run): InsuranceId =
        val now = run.currentTime
        val quantity = 10
        makePolicy(
          users.broker1,
          InsuranceCreateParams(
            icpPolicyHolder = users.policyHolder,
            icpPolicyPrice = BigInt(quantity) * 4 * 5 * 1_000_000,
            icpFidaCardQuantity = quantity,
            icpFidaCardValue = BigInt(1_000) * 1_000_000,
            icpPolicyAuthority = InsuranceAuthority.AtLeastOneSign(
              PList.from(Seq(users.fidaSystem.pkh, users.broker1.pkh))
            ),
            icpFundingDeadline = now + Time.days(7)
          )
        )

    /** Mints the policy NFTs under a fresh InsuranceId (parameterised by one of the broker's UTxOs,
      * which the transaction spends), locks the policy info and premium payment info at the
      * insurance policy script, funds one piggy bank per fida card, and loads the insurance policy
      * script as a reference script.
      */
    def makePolicy(broker: User, params: InsuranceCreateParams)(using run: Run): InsuranceId =
        import run.given
        val refUtxo = run.utxosAt(broker.address).head
        val ref = TxOutRef(TxId(refUtxo.input.transactionId), BigInt(refUtxo.input.index))
        val insuranceIdNFTScript = Scripts.insuranceIdMintingPolicyScript(ref)
        val iid = InsuranceId(insuranceIdNFTScript.scriptHash)
        val insuranceAddress = insurancePolicyAddress(iid)
        val cards = (1 to params.icpFidaCardQuantity).map(fidaCardFromInt)
        // Haskell `oneAda = adaValue 1`, i.e. one lovelace; the builder raises outputs to min ADA
        val oneAda = Value.lovelace(1)

        val minted: Map[AssetName, Long] =
            Map(
              AssetName(Tokens.policyInfoTokenName) -> 1L,
              AssetName(Tokens.policyPaymentTokenName) -> 1L,
              AssetName(Tokens.fidaCardStatusTokenName) -> params.icpFidaCardQuantity.toLong
            ) ++ cards.map(c => AssetName(Tokens.fidaCardTokenName(c.id)) -> 1L)

        val paymentInfo: InsurancePolicyDatum = PremiumPaymentInfo(
          ppInfoPremiumAmountPerPiggyBank = params.icpPolicyPrice / params.icpFidaCardQuantity,
          ppInfoPiggyBanks = PList.from(cards.map(c => plutusAddress(piggyBankAddr(iid, c))))
        )

        def fidaCardDatum(fcid: FidaCardId): PiggyBankDatum = PBankFidaCard(
          pbfcIsSold = false,
          pbfcFidaCardId = fcid,
          pbfcFidaCardValue = params.icpFidaCardValue,
          pbfcPaidClaims = PList.Nil
        )

        val iinfo: InsurancePolicyDatum = iinfoFromParams(params)
        val tx = cards.foldLeft(
          run.txBuilder
              .spend(refUtxo)
              .mint(insuranceIdNFTScript, minted, ())
              .payTo(insuranceAddress, oneAda + policyInfoNFT(iid), iinfo)
              .payTo(insuranceAddress, oneAda + policyPaymentNFT(iid), paymentInfo)
        ) { (b, fcid) =>
            b.payTo(
              piggyBankAddr(iid, fcid),
              oneAda + fidaCardStatusNFT(iid) + fidaCardNFT(iid, fcid),
              fidaCardDatum(fcid)
            )
        }
        run.submitTx(broker, tx)
        runLoadRefScript(broker, insurancePolicy(iid))
        iid

    /** Stores `script` as a reference script in an output at the script's own address. */
    def runLoadRefScript(user: User, script: Script.PlutusV2)(using run: Run): Unit =
        import run.given
        run.submitTx(
          user,
          run.txBuilder.output(
            TransactionOutput(
              scriptAddress(script),
              Value.lovelace(1),
              None,
              Some(ScriptRef(script))
            )
          )
        )
}
