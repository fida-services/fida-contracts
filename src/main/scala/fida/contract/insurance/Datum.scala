package fida.contract.insurance

import fida.contract.Utils.before
import scalus.compiler.{Compile, UplcRepr, UplcRepresentation}
import scalus.uplc.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Address, Interval, PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.prelude.*

/** Port of `Fida.Contract.Insurance.Datum`.
  *
  * Every type keeps the on-chain encoding produced by the Haskell `makeIsDataIndexed` /
  * newtype-deriving instances, so datums written by the existing off-chain code stay valid.
  * Constructor tags follow declaration order; record fields keep their Haskell names.
  */

/** Haskell `DiffMilliSeconds` (a newtype over `Integer`). */
type DiffMilliSeconds = BigInt

type PremiumAmount = BigInt

/** Identifier of a fida card. Haskell newtype over `BuiltinByteString`: encoded as a bare
  * bytestring.
  */
@UplcRepr(UplcRepresentation.ProductCaseOneElement)
case class FidaCardId(id: ByteString)

@Compile
object FidaCardId {
    given Eq[FidaCardId] = Eq.derived
    given ToData[FidaCardId] = (a: FidaCardId) => Builtins.bData(a.id)
    given FromData[FidaCardId] = (d: Data) => FidaCardId(Builtins.unBData(d))
}

enum InsurancePolicyState:
    case Initiated, Funding, OnRisk, Cancelled, Expired

@Compile
object InsurancePolicyState {
    given Eq[InsurancePolicyState] = Eq.derived
    given ToData[InsurancePolicyState] = ToData.derived
    given FromData[InsurancePolicyState] = FromData.derived
}

/** Haskell `InstallmentsInfo [DiffMilliSeconds] PremiumAmount`: payment intervals and the amount
  * unlocked per installment (for one fida card).
  */
case class InstallmentsInfo(
    paymentIntervals: List[DiffMilliSeconds],
    amountPerInstallment: PremiumAmount
)

@Compile
object InstallmentsInfo {
    given ToData[InstallmentsInfo] = ToData.derived
    given FromData[InstallmentsInfo] = FromData.derived
}

case class ClaimInfo(
    claimAmount: BigInt,
    claimDate: PosixTime,
    claimReason: ByteString,
    claimAccepted: Boolean,
    claimId: ByteString
)

@Compile
object ClaimInfo {
    given ToData[ClaimInfo] = ToData.derived
    given FromData[ClaimInfo] = FromData.derived
}

enum InsurancePolicyDatum:
    case InsuranceInfo(
        iInfoCollateralAmount: BigInt,
        iInfoFidaCardValue: BigInt,
        iInfoFidaCardNumber: BigInt,
        iInfoPremiumAmount: BigInt,
        iInfoInstallments: InstallmentsInfo,
        iInfoInsurancePeriod: DiffMilliSeconds,
        iInfoPolicyHolder: PubKeyHash,
        iInfoPolicyAuthority: InsuranceAuthority,
        iInfoState: InsurancePolicyState,
        iInfoFundingDeadline: PosixTime,
        iInfoTotalClaimsAcceptedAmount: BigInt,
        iInfoClaimTimeToLive: DiffMilliSeconds,
        iInfoClaimTimeToPay: DiffMilliSeconds,
        iInfoStartDate: Option[PosixTime],
        iInfoClaim: Option[ClaimInfo]
    )

    /** @param ppInfoPremiumAmountPerPiggyBank in lovelace */
    case PremiumPaymentInfo(
        ppInfoPremiumAmountPerPiggyBank: BigInt,
        ppInfoPiggyBanks: List[Address]
    )
    case PolicyClaimPayment

@Compile
object InsurancePolicyDatum {
    given ToData[InsurancePolicyDatum] = ToData.derived
    given FromData[InsurancePolicyDatum] = FromData.derived

    def updatePolicyState(
        datum: InsurancePolicyDatum,
        state: InsurancePolicyState
    ): Option[InsurancePolicyDatum] = datum match
        case info: InsuranceInfo => Option.Some(info.copy(iInfoState = state))
        case _                   => Option.None

    def untypedUpdatePolicyState(
        datum: InsurancePolicyDatum,
        state: InsurancePolicyState
    ): Option[Data] = updatePolicyState(datum, state).map(_.toData)

    def completeFunding(
        datum: InsurancePolicyDatum,
        startDate: PosixTime
    ): Option[InsurancePolicyDatum] = datum match
        case info: InsuranceInfo =>
            Option.Some(
              info.copy(
                iInfoState = InsurancePolicyState.OnRisk,
                iInfoStartDate = Option.Some(startDate)
              )
            )
        case _ => Option.None

    def updateClaim(
        datum: InsurancePolicyDatum,
        claim: Option[ClaimInfo]
    ): Option[InsurancePolicyDatum] = datum match
        case info: InsuranceInfo => Option.Some(info.copy(iInfoClaim = claim))
        case _                   => Option.None

    def untypedUpdateClaim(datum: InsurancePolicyDatum, claim: ClaimInfo): Option[Data] =
        updateClaim(datum, Option.Some(claim)).map(_.toData)

    def untypedUnsetClaim(datum: InsurancePolicyDatum): Option[Data] =
        updateClaim(datum, Option.None).map(_.toData)
}

enum PiggyBankDatum:
    /** Premium locked for investors (Haskell fields `pbankPremium'init`, `pbankPremium'refund`). */
    case PBankPremium(pbankPremiumInit: PremiumAmount, pbankPremiumRefund: PremiumAmount)
    case PBankFidaCard(
        pbfcIsSold: Boolean,
        pbfcFidaCardId: FidaCardId,
        pbfcFidaCardValue: BigInt,
        pbfcPaidClaims: List[ByteString]
    )

@Compile
object PiggyBankDatum {
    given ToData[PiggyBankDatum] = ToData.derived
    given FromData[PiggyBankDatum] = FromData.derived

    def updatePiggyBankRefund(amount: PremiumAmount, datum: PiggyBankDatum): Option[PiggyBankDatum] =
        datum match
            case premium: PBankPremium => Option.Some(premium.copy(pbankPremiumRefund = amount))
            case _                     => Option.None

    def untypedUpdatePiggyBankRefund(amount: PremiumAmount, datum: PiggyBankDatum): Option[Data] =
        updatePiggyBankRefund(amount, datum).map(_.toData)

    def addPiggyBankPaidClaim(claimId: ByteString, datum: PiggyBankDatum): Option[PiggyBankDatum] =
        datum match
            case card: PBankFidaCard =>
                Option.Some(card.copy(pbfcPaidClaims = List.Cons(claimId, card.pbfcPaidClaims)))
            case _ => Option.None

    def updatePiggyBankSoldFidaCardStatus(
        status: Boolean,
        datum: PiggyBankDatum
    ): Option[PiggyBankDatum] = datum match
        case card: PBankFidaCard => Option.Some(card.copy(pbfcIsSold = status))
        case _                   => Option.None

    def setFidaCardSold(datum: PiggyBankDatum): Option[PiggyBankDatum] =
        updatePiggyBankSoldFidaCardStatus(true, datum)

    def setFidaCardUnsold(datum: PiggyBankDatum): Option[PiggyBankDatum] =
        updatePiggyBankSoldFidaCardStatus(false, datum)

    def untypedSetFidaCardSold(datum: PiggyBankDatum): Option[Data] =
        setFidaCardSold(datum).map(_.toData)

    /** Premium (for one fida card) unlocked for investors at time `range`.
      *
      * Walks the installment schedule from `start`: each installment whose due date is strictly
      * before `range` unlocks `amountPerInstallment`; once every installment is due, the whole
      * `locked` amount is unlocked.
      */
    def unlockedPremiumToClaim(
        range: Interval,
        locked: PremiumAmount,
        installments: InstallmentsInfo,
        start: PosixTime
    ): PremiumAmount =
        def go(unlocked: PremiumAmount, date: PosixTime, intervals: List[DiffMilliSeconds]): PremiumAmount =
            intervals match
                case List.Nil => locked
                case List.Cons(dt, rest) =>
                    val nextDate = dt + date
                    if before(nextDate, range) then
                        go(unlocked + installments.amountPerInstallment, nextDate, rest)
                    else unlocked
        go(BigInt(0), start, installments.paymentIntervals)
}
