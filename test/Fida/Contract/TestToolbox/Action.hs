{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.TestToolbox.Action
  ( runUpdatePolicyState,
    updatePolicyStateTxRef,
    buyFidaCard,
    buyFidaCards,
    completeFundingTx,
    payPremium,
    sellFidaCard,
    payForClaimWithCollateral,
    triggerFundingComplete,
    unlockCollateralOnExpired,
    unlockCollateralOnCancel,
    unlockCollateralsOnExpired,
    triggerPolicyExpiration,
    claimPremium,
    claimPremiumOnCancel,
    module X,
  )
where

import Control.Monad (forM_)
import Data.Foldable (traverse_)
import Fida.Contract.Insurance.Datum
  ( FidaCardId (..),
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
    ClaimInfo(..),
    completeFunding,
    setFidaCardSold,
    setFidaCardUnsold,
    updatePolicyState,
    addPiggyBankPaidClaim,
    updatePiggyBankRefund,
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId(..))
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
    fidaCardsFromInts,
    pbPremiumBox
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
    refBoxInline,
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
    currentTime,
    validateIn,
    waitNSlots,
    boxAt,
    logInfo,
    Ada,
    ada,
    modifyBox,
    getLovelace
  )
import Fida.Contract.Utils (negateValue)
import Plutus.Model.Contract.Ext (payToAddressDatum, spendBoxRef)
import Plutus.V2.Ledger.Api (Address,
                             POSIXTime,
                             PubKeyHash, TxOut (..), TxOutRef, from, BuiltinByteString)
import Data.Maybe (fromMaybe)
import Prelude

runUpdatePolicyState ::
  InsurancePolicyState ->
  InsurancePolicyRedeemer ->
  InsuranceId ->
  PubKeyHash ->
  Run ()
runUpdatePolicyState state r iid pkh = do
  let tv = insurancePolicy iid
  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) -> do
    withBox @ InsurancePolicy (iinfoBox iid) tv $ \box -> do
      let maybeTx = updatePolicyStateTxRef scriptRef tv box state r
      withMay "Can't update policy state" (pure maybeTx) $ \tx -> do
        validRangeStart <- currentTime
        tx' <- validateIn (from validRangeStart) tx
        submitTx pkh tx'

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
  TxOutRef ->
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  POSIXTime ->
  Maybe Tx
completeFundingTx scriptRef tv box@(TxBox _ (TxOut _ value _ _) iinfo@InsuranceInfo {..}) onRiskStartDate =
  mkTx <$> completeFunding iinfo onRiskStartDate
 where
  mkTx iinfo =
    mconcat
      [ spendBoxRef scriptRef tv (PolicyFunding (PolicyFundingFundingComplete onRiskStartDate)) box
      , payToScript tv (InlineDatum iinfo) value
      ]

buyFidaCardTxRef ::
  TxOutRef ->
  InsuranceId ->
  PiggyBank ->
  TxBox PiggyBank ->
  PubKeyHash ->
  Maybe Tx
buyFidaCardTxRef scriptRef iid tv box@(TxBox _ (TxOut _ value _ _) pbank@PBankFidaCard {pbfcFidaCardValue, pbfcFidaCardId}) investor =
  mkTx <$> setFidaCardSold pbank
 where
  mkTx pbank' =
    mconcat
      [ spendBoxRef scriptRef tv BuyFidaCard box
      , payToScript tv (InlineDatum pbank') (value <> adaValue pbfcFidaCardValue <> (fidaCardNegateNFT iid pbfcFidaCardId))
      , payToKey investor (fidaCardNFT iid pbfcFidaCardId)
      ]

sellFidaCard ::
  InsuranceId ->
  PubKeyHash ->
  FidaCardId ->
  Run ()
sellFidaCard iid investor fcid = do
  let
    tv = piggyBank iid
    fidaCardValue = fidaCardNFT iid fcid
  sp <- spend investor fidaCardValue
  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) -> do
    withBox @PiggyBank (piggyBankInfoBox iid fcid) tv $
      \box@(TxBox _ (TxOut _ value _ _) pbank@PBankFidaCard {pbfcFidaCardValue}) ->
        withMay "Can't buy fida card" (pure $ setFidaCardUnsold pbank) $ \pbank' -> do
          let
            sellValue = adaValue pbfcFidaCardValue
            datum = InlineDatum pbank'
            value' = value <> negateValue sellValue <> fidaCardValue
            tx =
              mconcat
                [ spendBoxRef scriptRef tv SellFidaCard box
                , payToScript tv datum value'
                , payToKey investor sellValue
                , userSpend sp
                ]
          submitTx investor tx

buyFidaCard ::
  InsuranceId ->
  PubKeyHash ->
  FidaCardId ->
  Run ()
buyFidaCard iid investor fcid = do
  let tv = piggyBank iid
  sp <- spend investor $ adaValue 1_000_000_000

  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) -> do
    withBox @PiggyBank (piggyBankInfoBox iid fcid) tv $ \box -> do
      let maybeTx = buyFidaCardTxRef scriptRef iid tv box investor
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
  PubKeyHash ->
  Run ()
payPremium iid pkh = do
  let tv = insurancePolicy iid
  sp <- spend pkh $ adaValue 200_000_000
  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) ->
    withBox @InsurancePolicy (iinfoBox iid) tv $ \iiBox ->
      withBox @InsurancePolicy (ppInfoBox iid) tv $ \piBox -> do
        let r = PolicyInitiated PolicyInitiatedPayPremium
            maybeUpdateStTx = updatePolicyStateTxRef scriptRef tv iiBox Funding r
            maybePayToPiggyBanksTx = payPremiumToPiggyBanks scriptRef tv piBox pkh
            maybePayPremiumTx = (<>) <$> maybeUpdateStTx <*> maybePayToPiggyBanksTx
        withMay "Can't update policy state" (pure maybePayPremiumTx) $ \payPremiumTx -> do
          let tx =
                mconcat
                  [ payPremiumTx
                  , userSpend sp
                  ]
          submitTx pkh tx

payPremiumToPiggyBanks ::
  TxOutRef ->
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  PubKeyHash ->
  Maybe Tx
payPremiumToPiggyBanks scriptRef tv box@(TxBox _ (TxOut _ value _ _) PremiumPaymentInfo {..}) pkh =
  Just $ mconcat (payToPiggyBankTx <$> ppInfoFidaCardIds) <> spendPPaymentInfo
 where
  r = PolicyInitiated PolicyInitiatedPayPremium

  spendPPaymentInfo =
    mconcat
      [ spendBoxRef scriptRef tv r box
      , payToKey pkh value
      ]

  datum fidaCardId = InlineDatum $ PBankPremium ppInfoPremiumAmountPerPiggyBank 0 fidaCardId

  payToPiggyBankTx fidaCardId =
    payToAddressDatum ppInfoPiggyBankAddress (datum fidaCardId) (adaValue ppInfoPremiumAmountPerPiggyBank)
payPremiumToPiggyBanks _ _ _ _ = Nothing


addPiggyBankPaidClaimTx ::
  TxOutRef ->
  InsuranceId ->
  BuiltinByteString ->
  PiggyBank ->
  TxBox PiggyBank ->
  Integer ->
  Maybe Tx
addPiggyBankPaidClaimTx scriptRef iid claimId piggyBank box@(TxBox _ (TxOut _ value _ _) pbDatum) payAmount =
  mkTx <$> addPiggyBankPaidClaim claimId pbDatum
 where
  mkTx pbDatum' =
    mconcat
      [ spendBoxRef scriptRef piggyBank PayForClaimWithCollateral box
      , payToScript piggyBank (InlineDatum pbDatum') (value <> negateValue (adaValue payAmount))
      , payToScript (insurancePolicy iid) (InlineDatum PolicyClaimPayment) (adaValue payAmount)
      ]

payForClaimWithCollateral ::
  InsuranceId ->
  PubKeyHash ->
  Run ()
payForClaimWithCollateral iid@(InsuranceId cs) investor = do
  let tv = insurancePolicy iid
  withBox @InsurancePolicy (iinfoBox iid) tv $ \iInfoBox@(TxBox _ _ InsuranceInfo{iInfoFidaCardNumber, iInfoClaim = mClaim}) -> do
    withMay "Can't get the current claim" (pure mClaim) $ \claim@ClaimInfo{..} -> do
      forM_ (map fidaCardFromInt [1 .. iInfoFidaCardNumber]) $ \fcid@(FidaCardId fcid') -> do
              sp <- spend investor $ fidaCardNFT iid fcid
              withRefScript (isScriptRef (piggyBank iid)) (piggyBank iid) $ \(scriptRef, _) -> do
                withBox @PiggyBank (piggyBankInfoBox iid fcid) (piggyBank iid) $ \pBox@(TxBox _ _ pbDatum@PBankFidaCard{..}) -> do
                    let payAmount = claimAmount `div` iInfoFidaCardNumber

                    let maybeTx = addPiggyBankPaidClaimTx scriptRef iid claimId (piggyBank iid) pBox payAmount
                    withMay "Can't buy fida card" (pure maybeTx) $ \payForClaimWithCollateralTx -> do
                        let tx =
                                mconcat
                                  [ payForClaimWithCollateralTx
                                  , refBoxInline iInfoBox
                                  , userSpend sp
                                  , payToKey investor (fidaCardNFT iid fcid)
                                  ]
                        validRangeStart <- currentTime
                        tx' <- validateIn (from validRangeStart) tx
                        submitTx investor tx'


triggerFundingComplete :: InsuranceId -> Users -> Run ()
triggerFundingComplete iid users@Users {..} = do
  let tv = insurancePolicy iid

  onRiskStartDate <- currentTime

  waitNSlots 5

  actualStartTime <- currentTime

  let validRange = from actualStartTime

  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) -> do
    withBox @ InsurancePolicy (iinfoBox iid) tv $ \box@(TxBox _ _ InsuranceInfo {iInfoFidaCardNumber}) -> do
      let maybeUpdatePolicyStateTx = completeFundingTx scriptRef tv box onRiskStartDate

      allBoxes <- boxAt $ piggyBank iid

      let boxes = [box | box@(TxBox _ _ PBankFidaCard {..}) <- allBoxes]

      let refPiggyBanks = mconcat $ map refBoxInline boxes

      let maybeTx = (<>) <$> maybeUpdatePolicyStateTx <*> Just refPiggyBanks

      withMay "Can't update policy state" (pure maybeTx) $ \tx -> do
        tx' <- validateIn validRange tx
        (submitTx policyHolder tx')

    return ()


unlockCollateralsOnExpired :: PubKeyHash -> InsuranceId -> [FidaCardId] -> Run ()
unlockCollateralsOnExpired investor iid = traverse_ (unlockCollateralOnExpired investor iid)


unlockCollateralOnExpired :: PubKeyHash -> InsuranceId -> FidaCardId -> Run ()
unlockCollateralOnExpired = unlockCollateralOn UnlockCollateral


unlockCollateralOnCancel :: PubKeyHash -> InsuranceId -> FidaCardId -> Run ()
unlockCollateralOnCancel = unlockCollateralOn UnlockCollateralOnCancel


unlockCollateralOn :: PiggyBankRedeemer -> PubKeyHash -> InsuranceId -> FidaCardId -> Run ()
unlockCollateralOn r investor iid fcid = do
  let
    ipTv = insurancePolicy iid
    pbTv = piggyBank iid
    nft = fidaCardNFT iid fcid
  withBox @InsurancePolicy (iinfoBox iid) ipTv $ \iiBox@(TxBox _ _ d) ->
    withRefScript (isScriptRef pbTv) pbTv $ \(scriptRef, _) -> do
      withBox @PiggyBank (piggyBankInfoBox iid fcid) pbTv $ \pbBox@(TxBox _ (TxOut _ value _ _) _) -> do
        logInfo $ "policy info: " <> show d
        sp <- spend investor nft
        let tx = mconcat
              [ refBoxInline iiBox
              , userSpend sp
              , payToKey investor (value <> nft)
              , spendBoxRef scriptRef pbTv r pbBox
              ]
        submitTx investor tx


triggerPolicyExpiration :: InsuranceId -> PubKeyHash -> Run ()
triggerPolicyExpiration iid pkh = do
  let tv = insurancePolicy iid
  withRefScript (isScriptRef tv) tv $ \(scriptRef, _) ->
    withBox @InsurancePolicy (iinfoBox iid) tv $ \iiBox@(TxBox _ _ d) -> do
      let
        r = PolicyExpire
        maybeUpdateStateTx = updatePolicyStateTxRef scriptRef tv iiBox Expired r
      withMay "Can't update policy state" (pure maybeUpdateStateTx) $ \tx -> do
        validRangeStart <- currentTime
        validateIn (from validRangeStart) tx >>= submitTx pkh


claimPremium :: InsuranceId -> FidaCardId -> PubKeyHash -> Ada -> Run ()
claimPremium iid fcid pkh amount = do
  let
    ipTv = insurancePolicy iid
    pbTv = piggyBank iid
    nft = fidaCardNFT iid fcid
  withBox @InsurancePolicy (iinfoBox iid) ipTv $ \iiBox -> do
    withRefScript (isScriptRef pbTv) pbTv $ \(scriptRef, _) -> do
      withBox @PiggyBank pbPremiumBox pbTv $ \pbBox@(TxBox _ (TxOut _ value _ _) d) -> do
        sp <- spend pkh nft
        let
          newValue = value <> (negateValue $ ada amount)
          r = ClaimPremium
          tx' =
            if value == ada amount then spendBoxRef scriptRef pbTv r pbBox
            else mconcat
              [ spendBoxRef scriptRef pbTv r pbBox
              , payToScript pbTv (InlineDatum d) newValue
              ]
          tx = mconcat
              [ refBoxInline iiBox
              , userSpend sp
              , payToKey pkh (ada amount <> nft)
              , tx'
              ]
        validRangeStart <- currentTime
        validateIn (from validRangeStart) tx >>= submitTx pkh


claimPremiumOnCancel :: InsuranceId -> FidaCardId -> PubKeyHash -> Ada -> Run ()
claimPremiumOnCancel iid fcid pkh amount = do
  let
    ipTv = insurancePolicy iid
    pbTv = piggyBank iid
  withBox @InsurancePolicy (iinfoBox iid) ipTv $ \iiBox -> do
    withBox @PiggyBank pbPremiumBox pbTv $ \pbBox@(TxBox _ (TxOut _ value _ _) d) -> do
      logInfo $ "claim amount: " <> show (ada amount)
      logInfo $ "piggybank value: " <> show value
      logInfo $ "datum: " <> show d
      let
        modifyValue value = value <> (negateValue $ ada amount)
        updateDatum d = InlineDatum $ fromMaybe d (updatePiggyBankRefund (getLovelace amount) d)
        r = ClaimPremiumOnCancel
        tx' =
          if value == ada amount then spendBox pbTv r pbBox
          else modifyBox pbTv pbBox r updateDatum modifyValue
        tx = mconcat
             [ refBoxInline iiBox
             , payToKey pkh (ada amount)
             , tx'
             ]
      validRangeStart <- currentTime
      validateIn (from validRangeStart) tx >>= submitTx pkh
