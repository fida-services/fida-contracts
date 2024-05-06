module Fida.Contract.Insurance.Lifecycle.Funding (
    lifecycleFundingStateValidator,
) where

import Fida.Contract.Insurance.Datum (InsurancePolicyDatum)
import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer)
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude

{-# INLINEABLE lifecycleFundingStateValidator #-}
lifecycleFundingStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InsurancePolicyRedeemer ->
    ScriptContext ->
    Bool
lifecycleFundingStateValidator _ _ _ _ = True
