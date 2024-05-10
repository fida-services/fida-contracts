module Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator) where

import Fida.Contract.Insurance.Authority (isSignedByAuth)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..))
import Fida.Contract.Insurance.Identifier (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (InitStRedeemer (InitStCancell))
import Fida.Contract.Insurance.Tokens (insuranceDatumTokenName)
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
-}
{-# INLINEABLE lifecycleInitiatedStateValidator #-}
lifecycleInitiatedStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InitStRedeemer ->
    ScriptContext ->
    Bool
lifecycleInitiatedStateValidator (InsuranceId cs) datum@(InsuranceInfo{}) InitStCancell sc =
    traceIfFalse "ERROR-INITST-VALIDATOR-1" isSigned
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-2" verifyOut
  where
    isSigned = isSignedByAuth sc $ iInfoPolicyAuthority datum
    verifyOut :: [Bool]
    verifyOut =
        [ True
        | TxOut _ value (OutputDatum datum') _ <- getContinuingOutputs sc
        , valueOf value cs insuranceDatumTokenName == 1
        , getDatum datum' == PlutusTx.toBuiltinData datum{iInfoState = Cancelled}
        ]
lifecycleInitiatedStateValidator _ _ _ _ =
    trace "ERROR-INITST-VALIDATOR-0" False
