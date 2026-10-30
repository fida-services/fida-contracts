package fida.contract.insurance

import scalus.compiler.Compile
import scalus.uplc.builtin.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.PosixTime

/** Port of `Fida.Contract.Insurance.Redeemer`.
  *
  * Constructor tags follow declaration order and match the Haskell `makeIsDataIndexed` indices.
  */

/** Haskell `PolicyInitiatedRedemeer`: `PolicyInitiatedCancel` 0, `PolicyInitiatedPayPremium` 1. */
enum PolicyInitiatedRedeemer:
    case PolicyInitiatedCancel, PolicyInitiatedPayPremium

@Compile
object PolicyInitiatedRedeemer {
    given ToData[PolicyInitiatedRedeemer] = ToData.derived
    given FromData[PolicyInitiatedRedeemer] = FromData.derived
}

/** `PolicyFundingCancel` 0, `PolicyFundingFundingComplete` 1. */
enum PolicyFundingRedeemer:
    case PolicyFundingCancel
    case PolicyFundingFundingComplete(startDate: PosixTime)

@Compile
object PolicyFundingRedeemer {
    given ToData[PolicyFundingRedeemer] = ToData.derived
    given FromData[PolicyFundingRedeemer] = FromData.derived
}

/** Tags 0 to 7 in declaration order. */
enum PolicyOnRiskRedeemer:
    case PolicyOnRiskCreateClaim(claim: ClaimInfo)
    case PolicyOnRiskCloseClaim
    case PolicyOnRiskAcceptClaim
    case PolicyOnRiskFinalizeClaim
    case PolicyOnRiskExpireClaim
    case PolicyOnRiskFailClaim
    case PolicyOnRiskCancel
    case PolicyOnRiskClaimPayment

@Compile
object PolicyOnRiskRedeemer {
    given ToData[PolicyOnRiskRedeemer] = ToData.derived
    given FromData[PolicyOnRiskRedeemer] = FromData.derived
}

/** `PolicyInitiated` 0, `PolicyFunding` 1, `PolicyOnRisk` 2, `PolicyExpire` 3. */
enum InsurancePolicyRedeemer:
    case PolicyInitiated(redeemer: PolicyInitiatedRedeemer)
    case PolicyFunding(redeemer: PolicyFundingRedeemer)
    case PolicyOnRisk(redeemer: PolicyOnRiskRedeemer)
    case PolicyExpire

@Compile
object InsurancePolicyRedeemer {
    given ToData[InsurancePolicyRedeemer] = ToData.derived
    given FromData[InsurancePolicyRedeemer] = FromData.derived
}

/** Piggy bank actions.
  *
  * Declared in on-chain index order. The Haskell type declares `ClaimPremiumOnCancel` before
  * `PayForClaimWithCollateral`, but its `makeIsDataIndexed` assigns `PayForClaimWithCollateral`
  * index 3 and `ClaimPremiumOnCancel` index 4; the order below reproduces those indices.
  */
enum PiggyBankRedeemer:
    case BuyFidaCard // 0
    case SellFidaCard // 1
    case ClaimPremium // 2
    case PayForClaimWithCollateral // 3
    case ClaimPremiumOnCancel // 4
    case UnlockCollateralOnCancel // 5
    case UnlockCollateral // 6

@Compile
object PiggyBankRedeemer {
    given ToData[PiggyBankRedeemer] = ToData.derived
    given FromData[PiggyBankRedeemer] = FromData.derived
}
