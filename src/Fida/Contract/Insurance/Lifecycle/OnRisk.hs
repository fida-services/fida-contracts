{-# LANGUAGE NamedFieldPuns #-}

module Fida.Contract.Insurance.Lifecycle.OnRisk (
    lifecycleOnRiskStateValidator,
) where

import Fida.Contract.Insurance.Datum
    ( InsurancePolicyDatum(..)
    , ClaimInfo(..)
    , InsurancePolicyState(..)
    , untypedUpdateClaim, untypedUnsetClaim, untypedUpdatePolicyState
    )
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PolicyOnRiskRedeemer(..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Plutus.V2.Ledger.Api ( TxInfo(..), ScriptContext(..) )
import Plutus.V1.Ledger.Interval (before)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx.Prelude
import Fida.Contract.Utils (untypedOutputDatum, referenceDatums, traceIfNotSingleton)
import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Plutus.V1.Ledger.Time (fromMilliSeconds)

{- |
    ERROR-ON-RISK-VALIDATOR-0: Output datum doesn't match the updated datum

    ERROR-ON-RISK-VALIDATOR-1: Not signed by the policy holder

    ERROR-ON-RISK-VALIDATOR-2: Not signed by the policy authority

    ERROR-ON-RISK-VALIDATOR-3: Output datum doesn't match the updated datum

    ERROR-ON-RISK-VALIDATOR-4: Not signed by the policy holder

    ERROR-ON-RISK-VALIDATOR-5: Output datum doesn't match the updated datum

    ERROR-ON-RISK-VALIDATOR-6: Not signed by the policy authority

    ERROR-ON-RISK-VALIDATOR-7: Output datum doesn't match the updated datum

    ERROR-ON-RISK-VALIDATOR-8: Not signed by the policy authority

    ERROR-ON-RISK-VALIDATOR-9: Output datum doesn't match the updated datum

    ERROR-ON-RISK-VALIDATOR-10: Output datum doesn't match the updated datum

    ERROR-ON-RISK-VALIDATOR-11: Claim expired

    ERROR-ON-RISK-VALIDATOR-12: Not signed by the policy holder

-}
{-# INLINEABLE lifecycleOnRiskStateValidator #-}
lifecycleOnRiskStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    PolicyOnRiskRedeemer ->
    ScriptContext ->
    Bool
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState=OnRisk}) (PolicyOnRiskCreateClaim c@(ClaimInfo {claimAccepted, claimDate})) sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-0" correctOutputDatum
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-1" isSignedByPolicyHolder
    where
        isSignedByPolicyHolder = txSignedBy (scriptContextTxInfo sc) (iInfoPolicyHolder d)

        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        validRange = txInfoValidRange $ scriptContextTxInfo sc

        isClaimDateValid = before claimDate validRange

        correctOutputDatum =
            outputDatum == untypedUpdateClaim d c
            && claimAccepted == False
            && isClaimDateValid

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority, iInfoClaim=Just c}) PolicyOnRiskAcceptClaim sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-2" (isSignedByTheAuthority sc iInfoPolicyAuthority)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        correctOutputDatum = outputDatum == untypedUpdateClaim d c {claimAccepted = True}

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyHolder, iInfoClaim = Just _}) PolicyOnRiskCloseClaim sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-4" (txSignedBy txInfo iInfoPolicyHolder)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-5" correctOutputDatum
    where
      txInfo = scriptContextTxInfo sc

      outputDatum = untypedOutputDatum cs sc policyInfoTokenName

      correctOutputDatum = outputDatum == untypedUnsetClaim d

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority}) PolicyOnRiskFailClaim sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-6" (isSignedByTheAuthority sc iInfoPolicyAuthority)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-7" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        correctOutputDatum = outputDatum == untypedUnsetClaim d

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority}) PolicyOnRiskCancel sc =
  traceIfFalse "ERROR-ON-RISK-VALIDATOR-8" isSigned
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-9" correctOutput
  where
    isSigned = isSignedByTheAuthority sc iInfoPolicyAuthority

    outputDatum = untypedOutputDatum cs sc policyInfoTokenName

    correctOutput = outputDatum == untypedUpdatePolicyState d Cancelled

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoClaim=Just (ClaimInfo {claimDate}), iInfoClaimTimeToLive}) PolicyOnRiskExpireClaim sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-10" correctOutputDatum
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-11" claimExpired
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        correctOutputDatum = outputDatum == untypedUnsetClaim d

        claimDeadLine = claimDate + fromMilliSeconds iInfoClaimTimeToLive

        claimExpired = before claimDeadLine (txInfoValidRange $ scriptContextTxInfo sc)

lifecycleOnRiskStateValidator (InsuranceId cs) PolicyClaimPayment PolicyOnRiskClaimPayment sc =
    traceIfNotSingleton "ERROR-ON-RISK-VALIDATOR-12" isSignedByPolicyHolder
    where
      txInfo = scriptContextTxInfo sc

      isSignedByPolicyHolder =
        [ True
        | InsuranceInfo {iInfoPolicyHolder} <- referenceDatums cs sc policyInfoTokenName
        , txSignedBy txInfo iInfoPolicyHolder
        ]

lifecycleOnRiskStateValidator _ _ _ _ = False
