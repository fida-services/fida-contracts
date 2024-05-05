{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance
  ( serialisableInsurancePolicyValidator
  ) where

import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..))
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer)

import Plutus.V2.Ledger.Api (
    Script,
    ScriptContext,
    UnsafeFromData (unsafeFromBuiltinData),
    fromCompiledCode,
 )
import qualified PlutusTx
import PlutusTx.Prelude
import Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator)

{-# INLINEABLE mkInsurancePolicyValidator #-}
mkInsurancePolicyValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InsurancePolicyRedeemer ->
    ScriptContext ->
    Bool

mkInsurancePolicyValidator iid d@(InsuranceInfo{iInfoState = Initiated} ) r sc = lifecycleInitiatedStateValidator iid d r sc

mkInsurancePolicyValidator _ _ _ _ =
  trace "ERROR-INSURANCE-POLICY_VALIDATOR-0" False

{-# INLINEABLE mkInsurancePolicyValidatorUntyped #-}
mkInsurancePolicyValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkInsurancePolicyValidatorUntyped insuranceId datum redeemer sc =
    check $
        mkInsurancePolicyValidator
            (unsafeFromBuiltinData insuranceId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData sc)

serialisableInsurancePolicyValidator :: Script
serialisableInsurancePolicyValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkInsurancePolicyValidatorUntyped||])
