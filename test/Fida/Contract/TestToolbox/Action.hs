{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.TestToolbox.Action
  ( runUpdatePolicyState,
    updatePolicyStateTx,
    updatePolicyStateTxRef,
    buyFidaCard,
    buyFidaCards,
    completeFundingTx,
    payPremium,
    module X,
  )
where

import Fida.Contract.Insurance.Datum
  ( FidaCardId (..),
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
    completeFunding,
    setFidaCardSold,
    updatePolicyState,
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer
  ( InsurancePolicyRedeemer (..),
    PiggyBankRedeemer (..),
    PolicyFundingRedeemer (..),
    PolicyInitiatedRedemeer (..),
  )
import Fida.Contract.TestToolbox.Action.MakeInsurancePolicy as X
import Fida.Contract.TestToolbox.TypedValidators
  ( InsurancePolicy,
    PiggyBank,
    fidaCardFromInt,
    fidaCardNFT,
    fidaCardNegateNFT,
    fidaCardStatusNFT,
    fidaCardStatusNegateNFT,
    iinfoBox,
    insurancePolicy,
    isScriptRef,
    piggyBank,
    piggyBankInfoBox,
    ppInfoBox,
  )
import Fida.Contract.TestToolbox.Users (Users (..))
import Plutus.Model
  ( DatumMode (..),
    DatumType,
    HasAddress (..),
    HasDatum (..),
    Run,
    Tx,
    TxBox (..),
    UserSpend,
    adaValue,
    logError,
    payToKey,
    payToRef,
    payToScript,
    spend,
    spendBox,
    submitTx,
    userSpend,
    withBox,
    withMay,
    withRefScript,
  )
import Plutus.Model.Contract.Ext (payToAddressDatum, spendBoxRef)
import Plutus.V2.Ledger.Api (Address, POSIXTime, PubKeyHash, TxOut (..), TxOutRef)
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

completeFundingTx ::
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  POSIXTime ->
  Maybe Tx
completeFundingTx tv box@(TxBox _ (TxOut _ value _ _) iinfo@InsuranceInfo {..}) onRiskStartDate =
  mkTx <$> completeFunding iinfo onRiskStartDate
 where
  mkTx iinfo =
    mconcat
      [ spendBox tv (PolicyFunding (PolicyFundingFundingComplete onRiskStartDate)) box
      , payToScript tv (InlineDatum iinfo) value
      ]

buyFidaCardTx ::
  InsuranceId ->
  PiggyBank ->
  TxBox PiggyBank ->
  PubKeyHash ->
  Maybe Tx
buyFidaCardTx iid tv box@(TxBox _ (TxOut _ value _ _) pbank@PBankFidaCard {pbfcFidaCardValue, pbfcFidaCardId}) investor =
  mkTx <$> setFidaCardSold pbank
 where
  mkTx pbank' =
    mconcat
      [ spendBox tv BuyFidaCard box
      , payToScript tv (InlineDatum pbank') (value <> adaValue pbfcFidaCardValue <> (fidaCardNegateNFT iid pbfcFidaCardId))
      , payToKey investor (fidaCardNFT iid pbfcFidaCardId)
      ]

buyFidaCardTxRef ::
  TxOutRef ->
  PiggyBank ->
  TxBox PiggyBank ->
  Maybe Tx
buyFidaCardTxRef scriptRef tv box@(TxBox _ (TxOut _ value _ _) pbank@PBankFidaCard {pbfcFidaCardValue}) =
  mkTx <$> setFidaCardSold pbank
 where
  mkTx pbank' =
    mconcat
      [ spendBoxRef scriptRef tv BuyFidaCard box
      , payToRef tv (InlineDatum pbank') (value <> adaValue pbfcFidaCardValue)
      ]

buyFidaCard ::
  InsuranceId ->
  PubKeyHash ->
  FidaCardId ->
  Run ()
buyFidaCard iid investor fcid = do
  let tv = piggyBank iid fcid
  sp <- spend investor $ adaValue 1_000_000_000

  withBox @PiggyBank (piggyBankInfoBox iid fcid) tv $ \box -> do
    let maybeTx = buyFidaCardTx iid tv box investor
    withMay "Can't buy fida card" (pure maybeTx) $ \buyFidaCardTx -> do
      let tx =
            mconcat
              [ buyFidaCardTx
              , userSpend sp
              ]
      submitTx investor tx

buyFidaCards ::
  InsuranceId ->
  PubKeyHash ->
  [FidaCardId] ->
  Run ()
buyFidaCards iid investor fidaCardIds =
  mapM_ (buyFidaCard iid investor) fidaCardIds

payPremium ::
  InsuranceId ->
  Users ->
  Run ()
payPremium iid users@Users {..} = do
  let tv = insurancePolicy iid
  sp <- spend policyHolder $ adaValue 200_000_000
  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) ->
    withBox @InsurancePolicy (iinfoBox iid) tv $ \iiBox ->
      withBox @InsurancePolicy (ppInfoBox iid) tv $ \piBox -> do
        let r = PolicyInitiated PolicyInitiatedPayPremium
            maybeUpdateStTx = updatePolicyStateTxRef scriptRef tv iiBox Funding r
            maybePayToPiggyBanksTx = payPremiumToPiggyBanks scriptRef tv piBox policyHolder
            maybePayPremiumTx = (<>) <$> maybeUpdateStTx <*> maybePayToPiggyBanksTx
        withMay "Can't update policy state" (pure maybePayPremiumTx) $ \payPremiumTx -> do
          let tx =
                mconcat
                  [ payPremiumTx
                  , userSpend sp
                  ]
          submitTx policyHolder tx

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
