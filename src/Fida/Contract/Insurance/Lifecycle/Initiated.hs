{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator) where

import Fida.Contract.Insurance.Authority (isSignedByAuth)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..), updatePolicyState)
import Fida.Contract.Insurance.Identifier (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (InitStRedeemer (..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import Fida.Contract.Utils (traceIfNotSingleton)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Api as PlutusTx
import Plutus.V2.Ledger.Contexts
import PlutusTx.Prelude

{- |

 On chain errors:

  ERROR-INITST-VALIDATOR-0: TODO

  ERROR-INITST-VALIDATOR-1: TODO

  ERROR-INITST-VALIDATOR-2: TODO

  ERROR-INITST-VALIDATOR-3: TODO

  ERROR-INITST-VALIDATOR-4: TODO
-}
{-# INLINEABLE lifecycleInitiatedStateValidator #-}
lifecycleInitiatedStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InitStRedeemer ->
    ScriptContext ->
    Bool
lifecycleInitiatedStateValidator (InsuranceId cs) datum@(InsuranceInfo{iInfoState = Initiated}) InitStCancell sc =
    traceIfFalse "ERROR-INITST-VALIDATOR-1" isSigned
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-2" verifyOut
  where
    isSigned = isSignedByAuth sc $ iInfoPolicyAuthority datum
    verifyOut :: [Bool]
    verifyOut =
        [ True
        | TxOut _ value (OutputDatum datum') _ <- getContinuingOutputs sc
        , valueOf value cs policyInfoTokenName == 1
        , Just (getDatum datum') == fmap PlutusTx.toBuiltinData (updatePolicyState datum Cancelled)
        ]
lifecycleInitiatedStateValidator (InsuranceId cs) PremiumPaymentInfo{..} InitStPayPremium sc =
    traceIfFalse "ERROR-INITST-VALIDATOR-3" (all id verifyPayments)
        && traceIfFalse "ERROR-INITST-VALIDATOR-4" (length verifyPayments == length ppInfoPiggyBanks)
  where
    verifyPayments :: [Bool]
    verifyPayments = [False]
lifecycleInitiatedStateValidator _ _ _ _ =
    trace "ERROR-INITST-VALIDATOR-0" False
