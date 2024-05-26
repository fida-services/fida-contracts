{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance (
    serialisableInsurancePolicyValidator,
    insurancePolicyValidator
) where

import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..), updatePolicyState)
import Fida.Contract.Insurance.Identifier (InsuranceId(..))
import Fida.Contract.Utils (untypedOutputDatum, wrapValidator)
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Fida.Contract.Insurance.Lifecycle.Cancelled (lifecycleCancelledStateValidator)
import Fida.Contract.Insurance.Lifecycle.Funding (lifecycleFundingStateValidator)
import Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..))
import Plutus.V2.Ledger.Api (
    Script,
    ScriptContext (..),
    UnsafeFromData (unsafeFromBuiltinData),
    fromCompiledCode,
    txInfoValidRange, Validator, mkValidatorScript,
 )
import qualified PlutusTx
import Plutus.V1.Ledger.Interval (before)
import PlutusTx.Prelude

{-# INLINEABLE mkInsurancePolicyValidator #-}
mkInsurancePolicyValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InsurancePolicyRedeemer ->
    ScriptContext ->
    Bool
mkInsurancePolicyValidator iid d (PolicyInitiated r) sc = lifecycleInitiatedStateValidator iid d r sc
mkInsurancePolicyValidator iid d@(InsuranceInfo{iInfoState = Cancelled}) r sc =
    lifecycleCancelledStateValidator iid d r sc
mkInsurancePolicyValidator iid d@(InsuranceInfo{iInfoState = Funding}) (PolicyFunding r) sc =
    lifecycleFundingStateValidator iid d r sc
mkInsurancePolicyValidator (InsuranceId cs) d@(InsuranceInfo{iInfoExpireDate}) PolicyExpire sc =
    traceIfFalse "ERROR-INSURANCE-POLICY_VALIDATOR-0" (before iInfoExpireDate $ txInfoValidRange (scriptContextTxInfo sc))
    && traceIfFalse "ERROR-INSURANCE-POLICY_VALIDATOR-1" correctOutputDatum
    where
        outputDatum = untypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum = outputDatum == (PlutusTx.toBuiltinData <$> updatePolicyState d Expired)

mkInsurancePolicyValidator _ _ _ _ =
    trace "ERROR-INSURANCE-POLICY_VALIDATOR-0" False

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
insurancePolicyValidator iid = mkValidatorScript $
    $$(PlutusTx.compile [|| wrappedValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode iid
  where
    wrappedValidator = wrapValidator . mkInsurancePolicyValidator
