package fida.contract.toolbox

import fida.contract.Scripts
import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PremiumPaymentInfo}
import fida.contract.insurance.PiggyBankDatum.{PBankFidaCard, PBankPremium}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, CardanoInfo, Credential, LedgerToPlutusTranslation, PolicyId, Script, ScriptHash, Value}
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.v1

/** Scripts, addresses, tokens and box predicates (port of
  * `Fida.Contract.TestToolbox.TypedValidators`).
  */
object Validators {

    def insurancePolicy(iid: InsuranceId): Script.PlutusV2 = Scripts.insurancePolicyScript(iid)

    def piggyBank(iid: InsuranceId, fcid: FidaCardId): Script.PlutusV2 =
        Scripts.piggyBankScript(iid, fcid)

    def scriptAddress(script: Script.PlutusV2)(using env: CardanoInfo): Address =
        Address(env.network, Credential.ScriptHash(script.scriptHash))

    def insurancePolicyAddress(iid: InsuranceId)(using CardanoInfo): Address =
        scriptAddress(insurancePolicy(iid))

    def piggyBankAddr(iid: InsuranceId, fcid: FidaCardId)(using CardanoInfo): Address =
        scriptAddress(piggyBank(iid, fcid))

    /** The on-chain (Plutus) form of a ledger address. */
    def plutusAddress(address: Address): v1.Address = LedgerToPlutusTranslation.getAddress(address)

    def policyId(iid: InsuranceId): PolicyId = ScriptHash.fromByteString(iid.cs)

    def token(iid: InsuranceId, tokenName: ByteString, amount: Long): Value =
        Value.asset(policyId(iid), AssetName(tokenName), amount)

    def policyInfoNFT(iid: InsuranceId): Value = token(iid, Tokens.policyInfoTokenName, 1)

    def policyPaymentNFT(iid: InsuranceId): Value = token(iid, Tokens.policyPaymentTokenName, 1)

    def fidaCardNFT(iid: InsuranceId, fcid: FidaCardId): Value =
        token(iid, Tokens.fidaCardTokenName(fcid.id), 1)

    def fidaCardNegateNFT(iid: InsuranceId, fcid: FidaCardId): Value =
        token(iid, Tokens.fidaCardTokenName(fcid.id), -1)

    def fidaCardStatusNFT(iid: InsuranceId): Value = token(iid, Tokens.fidaCardStatusTokenName, 1)

    def fidaCardFromInt(n: Int): FidaCardId = FidaCardId(ByteString.fromString(n.toString))

    def fidaCardsFromInts(ns: Seq[Int]): Seq[FidaCardId] = ns.map(fidaCardFromInt)

    def quantity(value: Value, iid: InsuranceId, tokenName: ByteString): Long =
        value.assets.assets.get(policyId(iid)).flatMap(_.get(AssetName(tokenName))).getOrElse(0L)

    def lovelace(value: Value): BigInt = BigInt(value.coin.value)

    def iinfoBox(iid: InsuranceId)(box: Box[InsurancePolicyDatum]): Boolean =
        quantity(box.value, iid, Tokens.policyInfoTokenName) == 1

    def ppInfoBox(iid: InsuranceId)(box: Box[InsurancePolicyDatum]): Boolean =
        quantity(box.value, iid, Tokens.policyPaymentTokenName) == 1

    def piggyBankInfoBox(iid: InsuranceId, fcid: FidaCardId)(box: Box[PiggyBankDatum]): Boolean =
        box.datum match
            case card: PBankFidaCard =>
                quantity(box.value, iid, Tokens.fidaCardStatusTokenName) == 1 && card.pbfcFidaCardId == fcid
            case _ => false

    def pbPremiumBox(box: Box[PiggyBankDatum]): Boolean = box.datum match
        case _: PBankPremium => true
        case _               => false
}
