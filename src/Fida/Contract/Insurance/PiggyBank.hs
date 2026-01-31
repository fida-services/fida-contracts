{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.PiggyBank
  ( serialisablePiggyBankValidator,
    piggyBankValidator,
  )
where

import Fida.Contract.Insurance.Datum
  ( ClaimInfo (..),
    FidaCardId (..),
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
    unlockedPremiumToClaim,
    untypedSetFidaCardSold,
    untypedUpdatePiggyBankRefund,
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PiggyBankRedeemer (..))
import Fida.Contract.Insurance.Tokens
  ( fidaCardStatusTokenName,
    fidaCardTokenName,
    policyInfoTokenName,
  )
import Fida.Contract.Utils
  ( fromSingleton,
    lovelaceValueOf,
    output,
    outputDatum,
    referenceDatums,
    referenceOutputs,
    unsafeFromSingleton',
    untypedOutput,
    untypedOutputDatum,
    wrapValidator,
  )
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
  ( findOwnInput,
    getContinuingOutputs,
    txSignedBy,
    valueSpent,
  )
import qualified PlutusTx
import PlutusTx.Prelude

-- |
--
-- On chain errors:
--
--  ERR-PBANK-0: output datum doesn't say that the card is sold
--
--  ERR-PBANK-1: output datum doesn't match input datum
--
--  ERR-PBANK-2: own input not found
--
--  ERR-PBANK-3: correct output datum not found
--
--  ERR-PBANK-4: fida card not paid for
--
--  ERR-PBANK-5: output datum doesn't say that the card is not sold
--
--  ERR-PBANK-6: output datum doesn't match input datum
--
--  ERR-PBANK-7: not a fida card owner
--
--  ERR-PBANK-8: invalid claimed premium amount
--
--  ERR-PBANK-9: no reference input with insurance info
--
--  ERR-PBANK-10: insurance policy has not been started (no start date)
--
--  ERR-PBANK-11: claim is not accepted
--
--  ERR-PBANK-12: incorect collateral diff amount (to much collateral withdraw)
--
--  ERR-PBANK-13: claim was already paid
--
--  ERR-PBANK-14: claim was not marked as paid
--
--  ERR-PBANK-15: paid for claim was not correct
--
--  ERR-PBANK-16: unauthorised access
--
--  ERR-PBANK-17: fida card value can't be changed
--
--  ERR-PBANK-18: no reference input with insurance info
--
--  ERR-PBANK-19: own consumed input not found
--
--  ERR-PBANK-20: no output with correct datum
--
--  ERR-PBANK-21: policy is not cancelled
--
--  ERR-PBANK-22: not signed by the policy holder
--
--  ERR-PBANK-23: invalid claimed premium amount
--
--  ERR-PBANK-24: no reference input with insurance info
--
--  ERR-PBANK-25: policy is not cancelled
--
--  ERR-PBANK-26: not a fida card owner
--
--  ERR-PBANK-27: policy is not cancelled
--
--  ERR-PBANK-28: policy is not expired
--
--  ERR-PBANK-29: not a fida card owner
--
--  ERR-PBANK-30: claim was not paid
--
--  ERR-PBANK-31: no reference input with insurance info


{-# INLINEABLE mkPiggyBankValidator #-}
mkPiggyBankValidator ::
  InsuranceId ->
  FidaCardId ->
  PiggyBankDatum ->
  PiggyBankRedeemer ->
  ScriptContext ->
  Bool
mkPiggyBankValidator (InsuranceId cs) _ datum@(PBankFidaCard {pbfcIsSold = False, pbfcFidaCardValue}) BuyFidaCard sc =
  traceIfFalse "ERR-PBANK-0" isSold
 where
  inputLovelace = case findOwnInput sc of
    Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
    Nothing -> traceError "ERR-PBANK-2"

  isSold = Just outputDatum' == untypedSetFidaCardSold datum
  outputDatum' =
    case untypedOutput cs sc fidaCardStatusTokenName of
      Nothing -> traceError "ERR-PBANK-3"
      Just (TxOut _ v (OutputDatum (Datum d)) _)
        | lovelaceValueOf v >= pbfcFidaCardValue + inputLovelace -> d
        | otherwise -> traceError "ERR-PBANK-4"
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold = True, pbfcFidaCardValue}) SellFidaCard scriptContext =
  traceIfFalse "ERR-PBANK-5" (not pbfcIsSold)
    && traceIfFalse "ERR-PBANK-6" (pbfcFidaCardValue == pbfcFidaCardValue')
 where
  datum =
    fromSingleton
      [ datum'
      | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs scriptContext
      , valueOf value cs (fidaCardTokenName n) == 1
      , valueOf value cs fidaCardStatusTokenName == 1
      , Just datum' <- [PlutusTx.fromBuiltinData d]
      ]
  (pbfcIsSold, pbfcFidaCardValue') =
    case datum of
      Just (PBankFidaCard {pbfcIsSold = isSold, pbfcFidaCardValue = cardValue}) -> (isSold, cardValue)
      _ -> traceError "ERR-PBANK-8"
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) datum@(PBankPremium initAmount refund) ClaimPremium sc =
  traceIfFalse "ERR-PBANK-7" isFidaCardOwner
    && traceIfFalse "ERR-PBANK-8" isClaimedPremiumAmountValid
 where
  txInfo = scriptContextTxInfo sc
  isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

  lockedPremium =
    lovelaceValueOf . mconcat $
      [ value
      | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
      , d == toBuiltinData datum
      ]

  (maybePolicyStartDate, paymentIntervals) =
    unsafeFromSingleton'
      "ERR-PBANK-9"
      [ (iInfoStartDate, iInfoInstallments)
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

  availablePremium =
    case maybePolicyStartDate of
      Just start -> unlockedPremiumToClaim (txInfoValidRange txInfo) initAmount paymentIntervals start
      Nothing -> traceError "ERR-PBANK-10"

  isClaimedPremiumAmountValid = lockedPremium >= initAmount - refund - availablePremium
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold = True, pbfcFidaCardValue, pbfcPaidClaims}) PayForClaimWithCollateral sc =
  traceIfFalse "ERR-PBANK-11" isClaimAccepted
    && traceIfFalse "ERR-PBANK-12" collateralDiffAmountCorrect
    && traceIfFalse "ERR-PBANK-13" claimNotPaidYet
    && traceIfFalse "ERR-PBANK-14" claimMarkedAsPaid
    && traceIfFalse "ERR-PBANK-15" isPaidCorrect
    && traceIfFalse "ERR-PBANK-16" (isAfterClaimTimeToPay || isFidaCardOwner)
    && traceIfFalse "ERR-PBANK-17" isFidaCardValueTheSame
 where
  txInfo = scriptContextTxInfo sc

  (insuranceAddress, claimAmount, isClaimAccepted, claimId, claimDate, iInfoFidaCardNumber, iInfoClaimTimeToPay) =
    case referenceOutputs cs sc policyInfoTokenName of
      [(TxOut address _ _ _, d@InsuranceInfo {iInfoClaim = Just (ClaimInfo {claimAmount, claimAccepted, claimId, claimDate}), ..})] ->
        ( address
        , claimAmount
        , claimAccepted
        , claimId
        , claimDate
        , iInfoFidaCardNumber
        , iInfoClaimTimeToPay
        )
      _ -> traceError "ERR-PBANK-18"

  paid =
    lovelaceValueOf $
      mconcat
        [ value
        | TxOut address value (OutputDatum (Datum d)) _ <- txInfoOutputs txInfo
        , insuranceAddress == address
        , Just PolicyClaimPayment <- [fromBuiltinData d]
        ]

  inputCollateral = case findOwnInput sc of
    Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
    Nothing -> traceError "ERR-PBANK-19"

  (outputCollateral, paidClaims, pbfcFidaCardValue'') = case output cs sc fidaCardStatusTokenName of
    Just (TxOut _ value _ _, PBankFidaCard {pbfcPaidClaims = pbfcPaidClaims', pbfcFidaCardValue = pbfcFidaCardValue'}) -> (lovelaceValueOf value, pbfcPaidClaims', pbfcFidaCardValue')
    _ -> traceError "ERR-PBANK-20"

  collateralDiffAmountCorrect = claimAmount >= iInfoFidaCardNumber * (inputCollateral - outputCollateral)

  claimNotPaidYet = not (claimId `elem` pbfcPaidClaims)

  claimMarkedAsPaid = claimId : pbfcPaidClaims == paidClaims

  isPaidCorrect = iInfoFidaCardNumber * paid >= claimAmount

  isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

  isAfterClaimTimeToPay = before (claimDate + fromMilliSeconds iInfoClaimTimeToPay) $ txInfoValidRange txInfo

  isFidaCardValueTheSame = pbfcFidaCardValue'' == pbfcFidaCardValue

--
-- TODO ClaimPremiumOnCancel rename to RefundPremium
--
mkPiggyBankValidator (InsuranceId cs) _ datum@(PBankPremium initAmount refund) ClaimPremiumOnCancel sc =
  traceIfFalse "ERR-PBANK-21" isPolicyCancelled
    && traceIfFalse "ERR-PBANK-22" isSignedByPolicyHolder
    && traceIfFalse "ERR-PBANK-23" isClaimedPremiumAmountValid
    && traceIfFalse "ERR-PBANK-32" (refund == 0)
 where
  txInfo = scriptContextTxInfo sc

  (policyState, policyHolder, maybePolicyStartDate, paymentIntervals) =
    unsafeFromSingleton'
      "ERR-PBANK-24"
      [ (iInfoState, iInfoPolicyHolder, iInfoStartDate, iInfoInstallments)
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

  isPolicyCancelled = policyState == Cancelled

  isSignedByPolicyHolder = txSignedBy txInfo policyHolder

  spendValue =
    lovelaceValueOf . mconcat $
      [ value
      | Just (TxInInfo _ (TxOut _ value _ _)) <- [findOwnInput sc]
      ]

  lockedPremium =
    lovelaceValueOf . mconcat $
      [ value
      | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
      , Just d == untypedUpdatePiggyBankRefund refundAmount datum
      ]

  premiumLeftForInvestor =
    case maybePolicyStartDate of
      Just start -> unlockedPremiumToClaim (txInfoValidRange txInfo) initAmount paymentIntervals start
      Nothing -> 0

  refundAmount = initAmount - premiumLeftForInvestor

  isClaimedPremiumAmountValid = spendValue - lockedPremium <= refundAmount
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {}) UnlockCollateralOnCancel sc =
  traceIfFalse "ERR-PBANK-25" isPolicyCancelled
    && traceIfFalse "ERR-PBANK-26" isFidaCardOwner
 where
  txInfo = scriptContextTxInfo sc

  isPolicyCancelled =
    unsafeFromSingleton'
      "ERR-PBANK-27"
      [ iInfoState == Cancelled
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

  isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcPaidClaims}) UnlockCollateral sc =
  traceIfFalse "ERR-PBANK-28" isPolicyExpired
    && traceIfFalse "ERR-PBANK-29" isFidaCardOwner
    && traceIfFalse "ERR-PBANK-30" isClaimPaid
 where
  txInfo = scriptContextTxInfo sc

  (iInfoState, mClaim) =
    case
      [ (iInfoState, iInfoClaim)
      | iInfo@InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ] of
        [x] -> x
        _ -> traceError "ERR-PBANK-31"

  isPolicyExpired = iInfoState == Expired
  isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1
  isClaimPaid = fromMaybe True (((`elem` pbfcPaidClaims) . claimId) <$> mClaim)

mkPiggyBankValidator _ _ _ _ _ =
  traceError "ERR-PBANK-32"

{-# INLINEABLE mkPiggyBankValidatorUntyped #-}
mkPiggyBankValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkPiggyBankValidatorUntyped insuranceId fidaCardId =
  wrapValidator $
    mkPiggyBankValidator
      (unsafeFromBuiltinData insuranceId)
      (unsafeFromBuiltinData fidaCardId)

serialisablePiggyBankValidator :: Script
serialisablePiggyBankValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkPiggyBankValidatorUntyped||])

piggyBankValidator :: InsuranceId -> FidaCardId -> Validator
piggyBankValidator iid fcid =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode iid
      `PlutusTx.applyCode` PlutusTx.liftCode fcid
 where
  wrappedValidator iid' = wrapValidator . mkPiggyBankValidator iid'
