module Fida.Contract.TestToolbox.Action
  ( runUpdatePolicyState,
    updatePolicyStateTx,
    module X,
  )
where

import Fida.Contract.Insurance.Datum (InsurancePolicyState, updatePolicyState)
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer)
import Fida.Contract.TestToolbox.Action.MakeInsurancePolicy as X
import Fida.Contract.TestToolbox.TypedValidators (InsurancePolicy, iinfoBox, insurancePolicy)
import Plutus.Model
  ( DatumMode (..),
    Run,
    Tx,
    TxBox (..),
    logError,
    payToScript,
    spendBox,
    submitTx,
    withBox,
    withMay,
  )
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
  withBox @ InsurancePolicy (iinfoBox iid) tv $ \box -> do
    let maybeTx = updatePolicyStateTx tv box state r
    withMay "Can't update policy state" (pure maybeTx) (submitTx pkh)

updatePolicyStateTx ::
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  InsurancePolicyState ->
  InsurancePolicyRedeemer ->
  Maybe Tx
updatePolicyStateTx tv box@(TxBox _ (TxOut _ value _ _) iinfo) state r =
  mkTx <$> updatePolicyState iinfo state
 where
  mkTx iinfo =
    mconcat
      [ spendBox tv r box
      , payToScript tv (InlineDatum iinfo) value
      ]
