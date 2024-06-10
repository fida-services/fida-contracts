{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.TestToolbox.Action
  ( runUpdatePolicyState,
    updatePolicyStateTx,
    updatePolicyStateTxRef,
    payPremiumToPiggyBanks,
    module X,
  )
where

import Fida.Contract.Insurance.Datum (InsurancePolicyState, updatePolicyState, InsurancePolicyDatum (..),
                                     PiggyBankDatum (..))
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..), PolicyInitiatedRedemeer (..))
import Fida.Contract.TestToolbox.Action.MakeInsurancePolicy as X
import Fida.Contract.TestToolbox.TypedValidators (InsurancePolicy, iinfoBox, insurancePolicy)
import Plutus.Model
  ( DatumMode (..),
    Run,
    Tx,
    TxBox (..),
    payToScript,
    spendBox,
    submitTx,
    withBox,
    withMay,
    payToKey,
    adaValue
  )
import Plutus.Model.Contract.Ext (spendBoxRef, payToAddressDatum)
import Plutus.V2.Ledger.Api (PubKeyHash, TxOut (..), TxOutRef)
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
  mkTx iinfoDatum =
    mconcat
      [ spendBox tv r box
      , payToScript tv (InlineDatum iinfoDatum) value
      ]

updatePolicyStateTxRef ::
  TxOutRef ->
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  InsurancePolicyState ->
  InsurancePolicyRedeemer ->
  Maybe Tx
updatePolicyStateTxRef scriptRef tv box@(TxBox _ (TxOut _ value _ _) iinfo) state r =
  mkTx <$> updatePolicyState iinfo state
 where
  mkTx iinfoDatum =
    mconcat
      [ spendBoxRef scriptRef tv r box
      , payToScript tv (InlineDatum iinfoDatum) value
      ]

payPremiumToPiggyBanks ::
  TxOutRef ->
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  PubKeyHash ->
  Maybe Tx
payPremiumToPiggyBanks scriptRef tv box@(TxBox _ (TxOut _ value _ _) PremiumPaymentInfo {..}) pkh =
  Just $ mconcat (payToPiggyBankTx <$> ppInfoPiggyBanks) <> spendPPaymentInfo
 where
  r = PolicyInitiated PolicyInitiatedPayPremium

  spendPPaymentInfo =
    mconcat
      [ spendBoxRef scriptRef tv r box
      , payToKey pkh value
      ]

  datum = InlineDatum $ PBankPremium ppInfoPremiumAmountPerPiggyBank

  payToPiggyBankTx addr =
    payToAddressDatum addr datum (adaValue ppInfoPremiumAmountPerPiggyBank)
payPremiumToPiggyBanks _ _ _ _ = Nothing
