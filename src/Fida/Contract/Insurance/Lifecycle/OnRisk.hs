{-# LANGUAGE NamedFieldPuns #-}

module Fida.Contract.Insurance.Lifecycle.OnRisk
  ( lifecycleOnRiskStateValidator,
  )
where

import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Fida.Contract.Insurance.Datum
  ( ClaimInfo (..),
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    untypedUnsetClaim,
    untypedUpdateClaim,
    untypedUpdatePolicyState,
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PolicyOnRiskRedeemer (..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Fida.Contract.Utils (referenceDatums, traceIfNotSingleton, untypedOutputDatum)
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V2.Ledger.Api (ScriptContext (..), TxInfo (..))
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx.Prelude

-- |
--    ERR-ON-RISK-0: Output datum doesn't match the updated datum
--
--    ERR-ON-RISK-1: Not signed by the policy holder
--
--    ERR-ON-RISK-2: Not signed by the policy authority
--
--    ERR-ON-RISK-3: Output datum doesn't match the updated datum
--
--    ERR-ON-RISK-4: Not signed by the policy holder or by the policy authority
--
--    ERR-ON-RISK-5: Output datum doesn't match the updated datum
--
--    ERR-ON-RISK-6: Not signed by the policy authority
--
--    ERR-ON-RISK-7: Output datum doesn't match the updated datum
--
--    ERR-ON-RISK-8: Not signed by the policy authority
--
--    ERR-ON-RISK-9: Output datum doesn't match the updated datum
--
--    ERR-ON-RISK-10: Output datum doesn't match the updated datum
--
--    ERR-ON-RISK-11: Claim expired
--
--    ERR-ON-RISK-12: Not signed by the policy holder
{-# INLINEABLE lifecycleOnRiskStateValidator #-}
lifecycleOnRiskStateValidator ::
  InsuranceId ->
  InsurancePolicyDatum ->
  PolicyOnRiskRedeemer ->
  ScriptContext ->
  Bool
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoState = OnRisk}) (PolicyOnRiskCreateClaim c@(ClaimInfo {claimAccepted, claimDate})) sc =
  traceIfFalse "ERR-ON-RISK-0" correctOutputDatum
    && traceIfFalse "ERR-ON-RISK-1" isSignedByPolicyHolder
 where
  isSignedByPolicyHolder = txSignedBy (scriptContextTxInfo sc) (iInfoPolicyHolder d)

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  validRange = txInfoValidRange $ scriptContextTxInfo sc

  isClaimDateValid = before claimDate validRange

  correctOutputDatum =
    outputDatum == untypedUpdateClaim d c
      && claimAccepted == False
      && isClaimDateValid
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority, iInfoClaim = Just c}) PolicyOnRiskAcceptClaim sc =
  traceIfFalse "ERR-ON-RISK-2" (isSignedByTheAuthority sc iInfoPolicyAuthority)
    && traceIfFalse "ERR-ON-RISK-3" correctOutputDatum
 where
  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  correctOutputDatum = outputDatum == untypedUpdateClaim d c {claimAccepted = True}
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority, iInfoPolicyHolder, iInfoClaim = Just _}) PolicyOnRiskCloseClaim sc =
  traceIfFalse "ERR-ON-RISK-4" (isSignedByAuth || isSignedByPolicyHolder)
    && traceIfFalse "ERR-ON-RISK-5" correctOutputDatum
 where
  txInfo = scriptContextTxInfo sc

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  correctOutputDatum = outputDatum == untypedUnsetClaim d

  isSignedByAuth = isSignedByTheAuthority sc iInfoPolicyAuthority

  isSignedByPolicyHolder = txSignedBy txInfo iInfoPolicyHolder
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority}) PolicyOnRiskFailClaim sc =
  traceIfFalse "ERR-ON-RISK-6" (isSignedByTheAuthority sc iInfoPolicyAuthority)
    && traceIfFalse "ERR-ON-RISK-7" correctOutputDatum
 where
  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  correctOutputDatum = outputDatum == untypedUnsetClaim d
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoPolicyAuthority}) PolicyOnRiskCancel sc =
  traceIfFalse "ERR-ON-RISK-8" isSigned
    && traceIfFalse "ERR-ON-RISK-9" correctOutput
 where
  isSigned = isSignedByTheAuthority sc iInfoPolicyAuthority

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  correctOutput = outputDatum == untypedUpdatePolicyState d Cancelled
lifecycleOnRiskStateValidator (InsuranceId cs) d@(InsuranceInfo {iInfoClaim = Just (ClaimInfo {claimDate}), iInfoClaimTimeToLive}) PolicyOnRiskExpireClaim sc =
  traceIfFalse "ERR-ON-RISK-10" correctOutputDatum
    && traceIfFalse "ERR-ON-RISK-11" claimExpired
 where
  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  correctOutputDatum = outputDatum == untypedUnsetClaim d

  claimDeadLine = claimDate + fromMilliSeconds iInfoClaimTimeToLive

  claimExpired = before claimDeadLine (txInfoValidRange $ scriptContextTxInfo sc)
lifecycleOnRiskStateValidator (InsuranceId cs) PolicyClaimPayment PolicyOnRiskClaimPayment sc =
  traceIfNotSingleton "ERR-ON-RISK-12" isSignedByPolicyHolder
 where
  txInfo = scriptContextTxInfo sc

  isSignedByPolicyHolder =
    [ True
    | InsuranceInfo {iInfoPolicyHolder} <- referenceDatums cs sc policyInfoTokenName
    , txSignedBy txInfo iInfoPolicyHolder
    ]
lifecycleOnRiskStateValidator _ _ _ _ =
  trace "ERR-ON-RISK-13" False
