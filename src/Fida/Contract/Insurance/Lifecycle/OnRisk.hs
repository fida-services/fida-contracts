{-# LANGUAGE NamedFieldPuns #-}

module Fida.Contract.Insurance.Lifecycle.OnRisk (
    lifecycleOnRiskStateValidator,
) where

import Fida.Contract.Insurance.Datum
    ( InsurancePolicyDatum(..)
    , ClaimInfo(..)
    , InsurancePolicyState(..)
    , updateClaim
    )
import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Redeemer (PolicyOnRiskRedeemer(..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Plutus.V2.Ledger.Api
import Plutus.V1.Ledger.Interval (before)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx.Prelude
import PlutusTx as PlutsTx
import Fida.Contract.Utils (untypedOutputDatum, outputDatum)
import Fida.Contract.Insurance.Identifier (InsuranceId(..))
import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Plutus.V1.Ledger.Time (fromMilliSeconds)


{-# INLINEABLE lifecycleOnRiskStateValidator #-}
lifecycleOnRiskStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    PolicyOnRiskRedeemer ->
    ScriptContext ->
    Bool
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState}) (PolicyOnRiskCreateClaim c@(ClaimInfo {claimAccepted, claimDate})) sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-0" (iInfoState == OnRisk)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-1" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        validRange = txInfoValidRange $ scriptContextTxInfo sc
        isClaimDateValid = before claimDate validRange
        correctOutputDatum =
            outputDatum == (PlutsTx.toBuiltinData <$> updateClaim d (Just c))
            && claimAccepted == False
            && isClaimDateValid
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyAuthority, iInfoClaim=Just c}) (PolicyOnRiskAcceptClaim) sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-2" (isSignedByTheAuthority sc iInfoPolicyAuthority)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum = outputDatum == (PlutsTx.toBuiltinData <$> updateClaim d (Just (c {claimAccepted = True})))

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyHolder, iInfoClaim = Just c}) PolicyOnRiskCancelClaim sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-2" (txSignedBy (scriptContextTxInfo sc) iInfoPolicyHolder)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum = outputDatum == (PlutsTx.toBuiltinData <$> updateClaim d Nothing)

lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyAuthority}) (PolicyOnRiskFailClaim) sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-2" (isSignedByTheAuthority sc iInfoPolicyAuthority)
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum = outputDatum == (PlutsTx.toBuiltinData <$> updateClaim d Nothing)
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState, iInfoPolicyAuthority}) PolicyOnRiskCancel sc =
        traceIfFalse "ERROR-FUNDING-VALIDATOR-0" isSigned
    where
        isSigned = case outputDatum cs sc policyInfoTokenName of
            Just (InsuranceInfo{iInfoState = Cancelled}) -> isSignedByTheAuthority sc iInfoPolicyAuthority
            _ -> False
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoClaim=Just (ClaimInfo {claimDate}), iInfoState, iInfoPolicyAuthority, iInfoClaimTimeToLive}) PolicyOnRiskExpireClaim sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" correctOutputDatum
        && traceIfFalse "ERROR-ON-RISK-VALIDATOR-4" claimExpired
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum = outputDatum == (PlutsTx.toBuiltinData <$> updateClaim d Nothing)

        claimDeadLine = claimDate + fromMilliSeconds iInfoClaimTimeToLive

        claimExpired = before claimDeadLine (txInfoValidRange $ scriptContextTxInfo sc)

lifecycleOnRiskStateValidator (InsuranceId cs) PolicyClaimPayment PolicyOnRiskClaimPayment sc =
        traceIfFalse "ERROR-ON-RISK-VALIDATOR-3" isSignedByPolicyHolder
    where
        policyHolder = case outputDatum cs sc policyInfoTokenName of
            Nothing -> traceError "ERROR-ON-RISK-VALIDATOR-4"
            Just (InsuranceInfo{iInfoPolicyHolder}) -> iInfoPolicyHolder

        isSignedByPolicyHolder = txSignedBy (scriptContextTxInfo sc) policyHolder

lifecycleOnRiskStateValidator _ _ _ _ = False