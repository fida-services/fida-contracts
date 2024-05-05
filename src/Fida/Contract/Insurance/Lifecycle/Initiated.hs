module Fida.Contract.Insurance.Lifecycle.Initiated
  ( lifecycleInitiatedStateValidator ) where

import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer)
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude


{-# INLINEABLE lifecycleInitiatedStateValidator  #-}
lifecycleInitiatedStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InsurancePolicyRedeemer ->
    ScriptContext ->
    Bool
lifecycleInitiatedStateValidator _ _ _ _ = True
