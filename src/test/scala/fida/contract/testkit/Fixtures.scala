package fida.contract.testkit

import fida.contract.insurance.*
import fida.contract.insurance.InsurancePolicyDatum.{InsuranceInfo, PremiumPaymentInfo}
import fida.contract.testkit.ContextBuilder.*
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.v1.{PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.prelude.{List, Option}

/** Sample on-chain values mirroring the Haskell test toolbox (`MakeInsurancePolicy.hs`). */
object Fixtures {

    val day: BigInt = BigInt(24 * 60 * 60 * 1000)
    def days(n: Int): BigInt = day * n
    def seconds(n: Int): BigInt = BigInt(1000) * n

    /** Friday, 1 March 2024 12:12:12 (Haskell `beginningOfTime`). */
    val beginningOfTime: PosixTime = BigInt(1709295132000L)

    val insuranceId: InsuranceId = InsuranceId(ByteString.fromHex("ab" * 28))

    val fidaSystem: PubKeyHash = pkh(1)
    val broker1: PubKeyHash = pkh(2)
    val broker2: PubKeyHash = pkh(3)
    val policyHolder: PubKeyHash = pkh(4)
    val investor1: PubKeyHash = pkh(5)

    val fidaCardQuantity: BigInt = 10
    val fidaCardValue: BigInt = BigInt(1_000_000_000)
    val policyPrice: BigInt = fidaCardQuantity * 4 * 5 * 1_000_000

    /** `iinfoFromParams` for the sample policy (`newSamplePolicy`). */
    def sampleInsuranceInfo(
        state: InsurancePolicyState = InsurancePolicyState.Initiated,
        fundingDeadline: PosixTime = beginningOfTime + days(7),
        startDate: Option[PosixTime] = Option.None,
        claim: Option[ClaimInfo] = Option.None,
        authority: InsuranceAuthority =
            InsuranceAuthority.AtLeastOneSign(List.from(Seq(fidaSystem, broker1)))
    ): InsuranceInfo = InsuranceInfo(
      iInfoCollateralAmount = fidaCardQuantity * fidaCardValue,
      iInfoFidaCardValue = fidaCardValue,
      iInfoFidaCardNumber = fidaCardQuantity,
      iInfoPremiumAmount = policyPrice,
      iInfoInstallments = InstallmentsInfo(
        List.from(Seq(days(90), days(90), days(90))),
        policyPrice / fidaCardQuantity / 4
      ),
      iInfoInsurancePeriod = days(365),
      iInfoPolicyHolder = policyHolder,
      iInfoPolicyAuthority = authority,
      iInfoState = state,
      iInfoFundingDeadline = fundingDeadline,
      iInfoTotalClaimsAcceptedAmount = 0,
      iInfoClaimTimeToLive = seconds(100),
      iInfoClaimTimeToPay = seconds(50),
      iInfoStartDate = startDate,
      iInfoClaim = claim
    )

    def fidaCardId(n: Int): FidaCardId = FidaCardId(ByteString.fromString(n.toString))

    def samplePremiumPaymentInfo(): PremiumPaymentInfo =
        PremiumPaymentInfo(
          policyPrice / fidaCardQuantity,
          List.from((1 to fidaCardQuantity.toInt).map(i => scriptAddress(scriptHash(1000 + i))))
        )

    val sampleClaim: ClaimInfo = ClaimInfo(
      claimAmount = 10_000_000,
      claimDate = beginningOfTime + days(10),
      claimReason = ByteString.fromString("I'm sick!"),
      claimAccepted = false,
      claimId = ByteString.fromString("1234")
    )
}
