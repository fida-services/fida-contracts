module Fida.Contract.Insurance.Lifecycle.Cancelled
  ( lifecycleCancelledStateValidator,
  )
where

import Fida.Contract.Insurance.Datum (InsurancePolicyDatum)
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer)
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude

{-# INLINEABLE lifecycleCancelledStateValidator #-}
lifecycleCancelledStateValidator ::
  InsuranceId ->
  InsurancePolicyDatum ->
  InsurancePolicyRedeemer ->
  ScriptContext ->
  Bool
lifecycleCancelledStateValidator _ _ _ _ = trace "ERROR-CANCELLED-VALIDATOR-0" False
