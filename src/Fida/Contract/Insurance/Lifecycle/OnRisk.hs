module Fida.Contract.Insurance.Lifecycle.OnRisk (
    lifecycleOnRiskStateValidator,
) where

import Fida.Contract.Insurance.Datum (InsurancePolicyDatum)
import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Redeemer (PolicyOnRiskRedeemer)
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude

{-# INLINEABLE lifecycleOnRiskStateValidator #-}
lifecycleOnRiskStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    PolicyOnRiskRedeemer ->
    ScriptContext ->
    Bool
lifecycleOnRiskStateValidator _ _ _ _ = False
