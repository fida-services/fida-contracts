module Fida.Contract.TestToolbox.Action
  ( runUpdatePolicyState
  , module X
  ) where

import Fida.Contract.TestToolbox.Action.MakeInsurancePolicy as X
import Fida.Contract.TestToolbox.TypedValidators (InsurancePolicy, insurancePolicy, iinfoBox)
import Fida.Contract.Insurance.Datum (InsurancePolicyState, updatePolicyState)
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer)
import Plutus.Model (Run, TxBox (..), withBox, withMay, submitTx, spendBox, payToScript,  DatumMode (..))
import Plutus.V2.Ledger.Api (PubKeyHash, TxOut (..))
import Prelude

runUpdatePolicyState ::
  InsurancePolicyState ->
  InsurancePolicyRedeemer ->
  InsuranceId ->
  PubKeyHash ->
  Run ()
runUpdatePolicyState state r iid pkh = do
  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $
    \box@(TxBox _ (TxOut _ value _ _) iinfo) -> do
      let maybeIinfo = updatePolicyState iinfo state
      withMay "Can't update policy state" (pure maybeIinfo) $ \iinfo' -> do
        let tx =
              mconcat
                [ spendBox tv r box
                , payToScript tv (InlineDatum iinfo') value
                ]
        submitTx pkh tx
