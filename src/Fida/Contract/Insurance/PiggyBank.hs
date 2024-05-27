{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.PiggyBank (
    serialisablePiggyBankValidator,
) where

import Fida.Contract.Insurance.Datum (ClaimInfo(..), FidaCardId(..), PiggyBankDatum(..), InsurancePolicyDatum (..), InsurancePolicyState (Cancelled), unlockedPremiumToClaim)
import Fida.Contract.Insurance.InsuranceId (InsuranceId(..))
import Fida.Contract.Insurance.Redeemer (PiggyBankRedeemer(..))
import Fida.Contract.Insurance.Tokens (fidaCardTokenName, fidaCardStatusTokenName, policyInfoTokenName)
import Fida.Contract.Utils (fromSingleton, lovelaceValueOf, output, unsafeFromSingleton', referenceDatums, outputDatum, referenceOutputs)
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Contexts (getContinuingOutputs, findOwnInput, txSignedBy, valueSpent)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V1.Ledger.Interval (before)


{- |

 On chain errors:

  ERROR-PIGGY-BANK-VALIDATOR-0: output datum doesn't say that the card is sold

  ERROR-PIGGY-BANK-VALIDATOR-1: output datum doesn't match input datum

  ERROR-PIGGY-BANK-VALIDATOR-2: own input not found

  ERROR-PIGGY-BANK-VALIDATOR-3: correct output datum not found

  ERROR-PIGGY-BANK-VALIDATOR-4: fida card not paid for

  ERROR-PIGGY-BANK-VALIDATOR-5: output datum doesn't say that the card is not sold

  ERROR-PIGGY-BANK-VALIDATOR-6: output datum doesn't match input datum

  ERROR-PIGGY-BANK-VALIDATOR-7: not a fida card owner

  ERROR-PIGGY-BANK-VALIDATOR-8: invalid claimed premium amount

  ERROR-PIGGY-BANK-VALIDATOR-9: no reference input with insurance info

  ERROR-PIGGY-BANK-VALIDATOR-10: insurance policy has not been started (no start date)

  ERROR-PIGGY-BANK-VALIDATOR-11: claim is not accepted

  ERROR-PIGGY-BANK-VALIDATOR-12: incorect collateral diff amount (to much collateral withdraw)

  ERROR-PIGGY-BANK-VALIDATOR-13: claim was already paid

  ERROR-PIGGY-BANK-VALIDATOR-14: claim was not marked as paid

  ERROR-PIGGY-BANK-VALIDATOR-15: paid for claim was not correct

  ERROR-PIGGY-BANK-VALIDATOR-16: unauthorised access

  ERROR-PIGGY-BANK-VALIDATOR-17: fida card value can't be changed

  ERROR-PIGGY-BANK-VALIDATOR-18: no reference input with insurance info

  ERROR-PIGGY-BANK-VALIDATOR-19: own consumed input not found

  ERROR-PIGGY-BANK-VALIDATOR-20: no output with correct datum

  ERROR-PIGGY-BANK-VALIDATOR-21:

  ERROR-PIGGY-BANK-VALIDATOR-22:

-}
{-# INLINEABLE mkPiggyBankValidator #-}
mkPiggyBankValidator ::
    InsuranceId ->
    FidaCardId ->
    PiggyBankDatum ->
    PiggyBankRedeemer ->
    ScriptContext ->
    Bool
mkPiggyBankValidator (InsuranceId cs) _ (PBankFidaCard {pbfcIsSold=False, pbfcFidaCardValue}) BuyFidaCard scriptContext =
        traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-0" pbfcIsSold
        && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-1" (pbfcFidaCardValue == pbfcFidaCardValue')
    where
        inputLovelace = case findOwnInput scriptContext of
            Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
            Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-2"

        (PBankFidaCard {pbfcIsSold, pbfcFidaCardValue=pbfcFidaCardValue'}) =
                case outputDatum cs scriptContext fidaCardStatusTokenName of
                    Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-3"
                    Just (TxOut _ v _ _, d)
                      | lovelaceValueOf v >= pbfcFidaCardValue + inputLovelace -> d
                      | otherwise -> traceError "ERROR-PIGGY-BANK-VALIDATOR-4"


mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=True, pbfcFidaCardValue}) SellFidaCard scriptContext =
        traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-5" (not pbfcIsSold)
        && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-6" (pbfcFidaCardValue == pbfcFidaCardValue')
    where datum = fromSingleton
                    [ datum'
                    | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs scriptContext
                    , valueOf value cs (fidaCardTokenName n) == 1
                    , valueOf value cs fidaCardStatusTokenName == 1
                    , Just datum' <- [PlutusTx.fromBuiltinData d]
                    ]
          (pbfcIsSold, pbfcFidaCardValue') =
                case datum of
                    Just (PBankFidaCard {pbfcIsSold=isSold, pbfcFidaCardValue=cardValue }) -> (isSold, cardValue)
                    _ -> traceError "ERROR-PIGGY-BANK-VALIDATOR-8"
                    
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) datum@(PBankPremium initAmount) ClaimPremium sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-7" isFidaCardOwner
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-8" isClaimedPremiumAmountValid
  where
    txInfo = scriptContextTxInfo sc
    isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

    lockedPremium = lovelaceValueOf . mconcat $
      [ value
      | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
      , d == toBuiltinData datum
      ]

    (maybePolicyStartDate, paymentIntervals) =
      unsafeFromSingleton' "ERROR-PIGGY-BANK-VALIDATOR-9"
      [ (iInfoStartDate, iInfoInstallments)
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

    availablePremium =
      case maybePolicyStartDate of
        Just start -> unlockedPremiumToClaim (txInfoValidRange txInfo) initAmount paymentIntervals start
        Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-10"

    isClaimedPremiumAmountValid = lockedPremium >= initAmount - availablePremium

mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=True, pbfcFidaCardValue, pbfcPaidClaims}) PayForClaimWithCollateral sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-11" isClaimAccepted
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-12" collateralDiffAmountCorrect
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-13" claimNotPaidYet
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-14" claimMarkedAsPaid
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-15" isPaidCorrect
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-16" (isAfterClaimTimeToPay || isFidaCardOwner)
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-17" isFidaCardValueTheSame 
  where
    txInfo = scriptContextTxInfo sc

    ( insuranceAddress, claimAmount, isClaimAccepted, claimId, claimDate, iInfoFidaCardNumber, iInfoClaimTimeToPay) =
      case referenceOutputs cs sc policyInfoTokenName of
        [(TxOut address _ _ _, d@InsuranceInfo{iInfoClaim = Just (ClaimInfo {claimAmount, claimAccepted, claimId, claimDate}), ..})] ->
          ( address
          , claimAmount
          , claimAccepted
          , claimId
          , claimDate
          , iInfoFidaCardNumber
          , iInfoClaimTimeToPay
          )
        _ -> traceError "ERROR-PIGGY-BANK-VALIDATOR-18"


    paid = lovelaceValueOf $ mconcat
      [ value
      | TxOut address value (OutputDatum (Datum d)) _ <- txInfoOutputs txInfo
      , insuranceAddress == address
      , Just PolicyClaimPayment <- [fromBuiltinData d]
      ]

    inputCollateral = case findOwnInput sc of
            Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
            Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-19"

    (outputCollateral, paidClaims, pbfcFidaCardValue'') = case output cs sc fidaCardStatusTokenName of
            Just (TxOut _ value _ _, PBankFidaCard {pbfcPaidClaims=pbfcPaidClaims', pbfcFidaCardValue=pbfcFidaCardValue'}) -> (lovelaceValueOf value, pbfcPaidClaims', pbfcFidaCardValue')
            _ -> traceError "ERROR-PIGGY-BANK-VALIDATOR-20"

    collateralDiffAmountCorrect = claimAmount >= iInfoFidaCardNumber * (inputCollateral - outputCollateral)

    claimNotPaidYet = not (claimId `elem` pbfcPaidClaims)

    claimMarkedAsPaid = claimId : pbfcPaidClaims == paidClaims

    isPaidCorrect = iInfoFidaCardNumber * paid >= claimAmount

    isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

    isAfterClaimTimeToPay = before (claimDate + fromMilliSeconds iInfoClaimTimeToPay) $ txInfoValidRange txInfo

    isFidaCardValueTheSame = pbfcFidaCardValue'' == pbfcFidaCardValue

mkPiggyBankValidator (InsuranceId cs) _ datum@(PBankPremium initAmount) ClaimPremiumOnCancel sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-21" isPolicyCancelled
    && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-22" isSignedByPolicyHolder
    && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-23" isClaimedPremiumAmountValid
  where
    txInfo = scriptContextTxInfo sc

    (policyState, policyHolder, maybePolicyStartDate, paymentIntervals) =
      unsafeFromSingleton' "ERROR-PIGGY-BANK-VALIDATOR-24"
      [ (iInfoState, iInfoPolicyHolder, iInfoStartDate, iInfoInstallments)
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

    isPolicyCancelled = policyState == Cancelled

    isSignedByPolicyHolder = txSignedBy txInfo policyHolder

    lockedPremium = lovelaceValueOf . mconcat $
      [ value
      | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
      , d == toBuiltinData datum
      ]

    premiumLeftForInvestor =
      case maybePolicyStartDate of
        Just start -> unlockedPremiumToClaim (txInfoValidRange txInfo) initAmount paymentIntervals start
        Nothing -> initAmount

    isClaimedPremiumAmountValid = lockedPremium >= premiumLeftForInvestor

mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {}) UnlockCollateralOnCancel sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-13" isPolicyCancelled
    && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-14" isFidaCardOwner
  where
    txInfo = scriptContextTxInfo sc

    isPolicyCancelled =
      unsafeFromSingleton' "ERROR-PIGGY-BANK-VALIDATOR-15"
      [ iInfoState == Cancelled
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

    isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

mkPiggyBankValidator _ _ _ _ _ = False


{-# INLINEABLE mkPiggyBankValidatorUntyped #-}
mkPiggyBankValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkPiggyBankValidatorUntyped insuranceId fidaCardId datum redeemer sc =
    check $
        mkPiggyBankValidator
            (unsafeFromBuiltinData insuranceId)
            (unsafeFromBuiltinData fidaCardId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData sc)

serialisablePiggyBankValidator :: Script
serialisablePiggyBankValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkPiggyBankValidatorUntyped||])
