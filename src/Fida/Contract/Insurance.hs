{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance
  ( serialisableInsurancePolicyValidator,
    insurancePolicyValidator,
  )
where

import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..), untypedUpdatePolicyState)
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Lifecycle.Cancelled (lifecycleCancelledStateValidator)
import Fida.Contract.Insurance.Lifecycle.Funding (lifecycleFundingStateValidator)
import Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator)
import Fida.Contract.Insurance.Lifecycle.OnRisk (lifecycleOnRiskStateValidator)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Fida.Contract.Utils (unsafeFromJust, untypedOutputDatum, wrapValidator)
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V2.Ledger.Api
  ( Script,
    ScriptContext (..),
    UnsafeFromData (unsafeFromBuiltinData),
    Validator,
    fromCompiledCode,
    mkValidatorScript,
    txInfoValidRange,
  )
import qualified PlutusTx
import PlutusTx.Prelude

-- |
--
--  TODO: Add description
--
--  On chain errors:
--
--    ERROR-INSURANCE-MAIN-VALIDATOR-0: illegal action for main validator
--
--    ERROR-INSURANCE-MAIN-VALIDATOR-1: TODO
--
--    ERROR-INSURANCE-MAIN-VALIDATOR-2: TODO
--
--    ERROR-INSURANCE-MAIN-VALIDATOR-3: TODO
{-# INLINEABLE mkInsurancePolicyValidator #-}
mkInsurancePolicyValidator ::
  InsuranceId ->
  InsurancePolicyDatum ->
  InsurancePolicyRedeemer ->
  ScriptContext ->
  Bool
mkInsurancePolicyValidator iid d (PolicyInitiated r) sc =
  lifecycleInitiatedStateValidator iid d r sc
mkInsurancePolicyValidator iid d@(InsuranceInfo {iInfoState = Cancelled}) r sc =
  lifecycleCancelledStateValidator iid d r sc
mkInsurancePolicyValidator iid d (PolicyFunding r) sc =
  lifecycleFundingStateValidator iid d r sc
mkInsurancePolicyValidator iid d (PolicyOnRisk r) sc =
  lifecycleOnRiskStateValidator iid d r sc
mkInsurancePolicyValidator (InsuranceId cs) d@(InsuranceInfo {iInfoStartDate, iInfoInsurancePeriod, iInfoState, iInfoFundingDeadline}) PolicyExpire sc =
    traceIfFalse "ERROR-INSURANCE-MAIN-VALIDATOR-2" hasExpired
    && traceIfFalse "ERROR-INSURANCE-MAIN-VALIDATOR-3" correctOutputDatum
 where
  txInfo = scriptContextTxInfo sc

  expireDate =
    case (iInfoState, iInfoStartDate) of
      (Funding, _)             -> iInfoFundingDeadline
      (OnRisk, Just startDate)  -> startDate + fromMilliSeconds iInfoInsurancePeriod
      _ -> traceError "ERROR-INSURANCE-MAIN-VALIDATOR-1"

  hasExpired = expireDate `before` txInfoValidRange txInfo

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  correctOutputDatum = outputDatum == untypedUpdatePolicyState d Expired

mkInsurancePolicyValidator _ _ _ _ =
  trace "ERROR-INSURANCE-MAIN-VALIDATOR-0" False

{-# INLINEABLE mkInsurancePolicyValidatorUntyped #-}
mkInsurancePolicyValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkInsurancePolicyValidatorUntyped insuranceId =
  wrapValidator $
    mkInsurancePolicyValidator
      (unsafeFromBuiltinData insuranceId)

serialisableInsurancePolicyValidator :: Script
serialisableInsurancePolicyValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkInsurancePolicyValidatorUntyped||])

insurancePolicyValidator :: InsuranceId -> Validator
insurancePolicyValidator iid =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode iid
 where
  wrappedValidator = wrapValidator . mkInsurancePolicyValidator
