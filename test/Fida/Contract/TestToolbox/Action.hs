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

import Fida.Contract.Insurance.Datum (InsurancePolicyState, FidaCardId(..), completeFunding, updatePiggyBankFidaCardStatus, updatePolicyState, PiggyBankDatum(..), InsurancePolicyState(..), InsurancePolicyDatum(..))
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer(..), PolicyInitiatedRedemeer(..), PiggyBankRedeemer(..), PolicyFundingRedeemer(..))
import Fida.Contract.TestToolbox.Action.MakeInsurancePolicy as X
import Fida.Contract.TestToolbox.TypedValidators (InsurancePolicy, PiggyBank, fidaCardStatusNFT, piggyBank, fidaCardNFT, fidaCardNegateNFT,fidaCardStatusNegateNFT, fidaCardFromInt, iinfoBox, ppInfoBox, insurancePolicy, piggyBankInfoBox, isScriptRef)
import Fida.Contract.TestToolbox.Users (Users(..))
import Plutus.Model
  ( DatumMode (..),
    HasDatum (..),
    Run,
    HasAddress (..),
    Tx,
    TxBox (..),
    UserSpend,
    DatumType,
    userSpend,
    payToRef,
    adaValue,
    logError,
    payToScript,
    spendBox,
    submitTx,
    withBox,
    withMay,
    payToKey,
    spend,
    withRefScript
  )
import Plutus.Model.Contract.Ext (spendBoxRef, payToAddressDatum)
import Plutus.V2.Ledger.Api (PubKeyHash, TxOut (..), TxOutRef, Address, POSIXTime)
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
completeFundingTx tv box@(TxBox _ (TxOut _ value _ _) iinfo@InsuranceInfo{..}) onRiskStartDate =
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
buyFidaCardTx iid tv box@(TxBox _ (TxOut _ value _ _) pbank@PBankFidaCard{pbfcFidaCardValue, pbfcFidaCardId}) investor =
  mkTx <$> updatePiggyBankFidaCardStatus pbank True
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
buyFidaCardTxRef scriptRef tv box@(TxBox _ (TxOut _ value _ _) pbank@PBankFidaCard{pbfcFidaCardValue}) =
  mkTx <$> updatePiggyBankFidaCardStatus pbank True
 where
  mkTx pbank' =
    mconcat
      [ spendBoxRef scriptRef tv BuyFidaCard box
      , payToRef tv (InlineDatum pbank') (value <> adaValue pbfcFidaCardValue)
      ]

buyFidaCard ::
  InsuranceId ->
  FidaCardId ->
  Users ->
  Run ()
buyFidaCard iid fcid users@Users{..} = do
  let tv = piggyBank iid fcid
  sp <- spend investor1 $ adaValue 1_000_000_000

  withBox @PiggyBank (piggyBankInfoBox iid fcid) tv $ \box -> do
      let maybeTx = buyFidaCardTx iid tv box investor1
      withMay "Can't buy fida card" (pure maybeTx) $ \buyFidaCardTx -> do
        let tx =
              mconcat
                [ buyFidaCardTx
                , userSpend sp
                ]
        submitTx investor1 tx

buyFidaCards ::
  InsuranceId ->
  Users ->
  Run ()
buyFidaCards iid users@Users{..} = do
  withBox @InsurancePolicy (iinfoBox iid) (insurancePolicy iid) $ \(TxBox _ _ InsuranceInfo{iInfoFidaCardNumber}) -> do
    let fidaCardIds = [1 .. iInfoFidaCardNumber]
    mapM_ (\fcId -> buyFidaCard iid (fidaCardFromInt fcId) users) fidaCardIds

payPremium ::
  InsuranceId ->
  Users ->
  Run ()
payPremium iid users@Users{..} = do
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


newtype PiggyBankAddress = PiggyBankAddress Address


instance HasDatum PiggyBankAddress where
  type DatumType PiggyBankAddress = PiggyBankDatum

instance HasAddress PiggyBankAddress where
  toAddress (PiggyBankAddress addr) = addr

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
