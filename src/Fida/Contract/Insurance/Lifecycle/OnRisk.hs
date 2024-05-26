{-# LANGUAGE NamedFieldPuns #-}

module Fida.Contract.Insurance.Lifecycle.OnRisk (
    lifecycleOnRiskStateValidator,
) where

import Fida.Contract.Insurance.Datum
    ( InsurancePolicyDatum(..)
    , ClaimInfo(..)
    , InsurancePolicyState(..)
    , updateClaim
    , updatePolicyState, untypedUpdateClaim, untypedUnsetClaim, untypedUpdatePolicyState
    )
import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Redeemer (PolicyOnRiskRedeemer(..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Plutus.V2.Ledger.Api
import Plutus.V1.Ledger.Interval (before)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx.Prelude
import PlutusTx as PlutusTx
import Fida.Contract.Utils (untypedOutputDatum, outputDatum, referenceDatums)
import Fida.Contract.Insurance.Identifier (InsuranceId(..))
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

    ERROR-ON-RISK-VALIDATOR-13: Reference InsuranceInfo utxo not found
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

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyAuthority, iInfoClaim=Just c}) (PolicyOnRiskAcceptClaim) sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-2" (isSignedByTheAuthority sc iInfoPolicyAuthority)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        correctOutputDatum = outputDatum == untypedUpdateClaim d c {claimAccepted = True}

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyHolder, iInfoClaim = Just c}) PolicyOnRiskCloseClaim sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-4" (txSignedBy (scriptContextTxInfo sc) iInfoPolicyHolder)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-5" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        correctOutputDatum = outputDatum == untypedUnsetClaim d

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyAuthority}) (PolicyOnRiskFailClaim) sc =
    traceIfFalse "ERROR-ON-RISK-VALIDATOR-6" (isSignedByTheAuthority sc iInfoPolicyAuthority)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-7" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName

        correctOutputDatum = outputDatum == untypedUnsetClaim d

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyAuthority}) PolicyOnRiskCancel sc =
  traceIfFalse "ERROR-ON-RISK-VALIDATOR-8" isSigned
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-9" correctOutput
  where
    isSigned = isSignedByTheAuthority sc iInfoPolicyAuthority

    outputDatum = untypedOutputDatum cs sc policyInfoTokenName

    correctOutput = outputDatum == untypedUpdatePolicyState d Cancelled

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoClaim=Just (ClaimInfo {claimDate}), iInfoState, iInfoPolicyAuthority, iInfoClaimTimeToLive}) PolicyOnRiskExpireClaim sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-10" correctOutputDatum
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-11" claimExpired
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum = outputDatum == (PlutusTx.toBuiltinData <$> updateClaim d Nothing)

        claimDeadLine = claimDate + fromMilliSeconds iInfoClaimTimeToLive

        claimExpired = before claimDeadLine (txInfoValidRange $ scriptContextTxInfo sc)

lifecycleOnRiskStateValidator (InsuranceId cs) PolicyClaimPayment PolicyOnRiskClaimPayment sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-12" isSignedByPolicyHolder
    where
        policyHolder = case referenceDatums cs sc policyInfoTokenName of
            [InsuranceInfo {iInfoPolicyHolder}] -> iInfoPolicyHolder
            _ -> traceError "ERROR-ON-RISK-VALIDATOR-13"

        isSignedByPolicyHolder = txSignedBy (scriptContextTxInfo sc) policyHolder

lifecycleOnRiskStateValidator _ _ _ _ = False
