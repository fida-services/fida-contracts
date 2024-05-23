{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.PiggyBank (
    serialisablePiggyBankValidator,
) where

import Fida.Contract.Insurance.Datum (ClaimInfo(..), FidaCardId(..), PiggyBankDatum(..), InsurancePolicyDatum (..), InsurancePolicyState (Cancelled), unlockedPremiumToClaim)
import Fida.Contract.Insurance.Identifier (InsuranceId(..))
import Fida.Contract.Insurance.Redeemer (PiggyBankRedeemer(..))
import Fida.Contract.Insurance.Tokens (fidaCardTokenName, fidaCardStatusTokenName, policyInfoTokenName)
import Fida.Contract.Utils (unsafeReferenceDatum, fromSingleton, lovelaceValueOf, output, unsafeFromSingleton', referenceDatums, outputDatum, unsafeReferenceOutput)
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

  ERROR-PIGGY-BANK-VALIDATOR-5: output datum not found

  ERROR-PIGGY-BANK-VALIDATOR-6: output datum says that the card is sold

  ERROR-PIGGY-BANK-VALIDATOR-7: output datum doesn't match input datum

  ERROR-PIGGY-BANK-VALIDATOR-8: datum not found

  ERROR-PIGGY-BANK-VALIDATOR-9: TODO

  ERROR-PIGGY-BANK-VALIDATOR-10: TODO

  ERROR-PIGGY-BANK-VALIDATOR-11: TODO

  ERROR-PIGGY-BANK-VALIDATOR-12: TODO

  ERROR-PIGGY-BANK-VALIDATOR-13: TODO

  ERROR-PIGGY-BANK-VALIDATOR-14: TODO

  ERROR-PIGGY-BANK-VALIDATOR-15: TODO

-}
{-# INLINEABLE mkPiggyBankValidator #-}
mkPiggyBankValidator ::
    InsuranceId ->
    FidaCardId ->
    PiggyBankDatum ->
    PiggyBankRedeemer ->
    ScriptContext ->
    Bool
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=False, pbfcFidaCardValue}) BuyFidaCard scriptContext =
        traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-0" pbfcIsSold
        && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-1" (pbfcFidaCardValue == pbfcFidaCardValue')
    where
        inputLovelace = case findOwnInput scriptContext of
            Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
            Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-2"

        (PBankFidaCard {pbfcIsSold, pbfcFidaCardValue=pbfcFidaCardValue'}) =
                case outputDatum cs scriptContext fidaCardStatusTokenName of
                    Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-3"
                    Just (TxOut _ v _ _, d) | lovelaceValueOf v < pbfcFidaCardValue + inputLovelace ->
                        traceError "ERROR-PIGGY-BANK-VALIDATOR-4"
                                            | otherwise -> d
                    _ -> traceError "ERROR-PIGGY-BANK-VALIDATOR-5"


mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=True, pbfcFidaCardValue}) SellFidaCard scriptContext =
        traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-6" (not pbfcIsSold)
        && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-7" (pbfcFidaCardValue == pbfcFidaCardValue')
    where datum = fromSingleton
                    [ datum
                    | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs scriptContext
                    , valueOf value cs (fidaCardTokenName n) == 1
                    , valueOf value cs fidaCardStatusTokenName == 1
                    , Just datum <- [PlutusTx.fromBuiltinData d]
                    ]
          (PBankFidaCard {pbfcIsSold, pbfcFidaCardValue=pbfcFidaCardValue'}) =
                case datum of
                    Just x -> x
                    Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-8"
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) datum@(PBankPremium initAmount) ClaimPremium sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-16" isFidaCardOwner
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-17" isClaimedPremiumAmountValid
  where
    txInfo = scriptContextTxInfo sc
    isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

    lockedPremium = lovelaceValueOf . mconcat $
      [ value
      | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
      , d == toBuiltinData datum
      ]

    (policyState, policyHolder, maybePolicyStartDate, paymentIntervals) =
      unsafeFromSingleton' "ERROR-PIGGY-BANK-VALIDATOR-18"
      [ (iInfoState, iInfoPolicyHolder, iInfoStartDate, iInfoInstallments)
      | InsuranceInfo {..} <- referenceDatums cs sc policyInfoTokenName
      ]

    unlockedPremium =
      case maybePolicyStartDate of
        Just start -> unlockedPremiumToClaim (txInfoValidRange txInfo) initAmount paymentIntervals start
        Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-19"

    isClaimedPremiumAmountValid = lockedPremium >= initAmount - unlockedPremium

mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=True, pbfcFidaCardValue, pbfcPaidClaims}) PayForClaimWithCollateral sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-24" isClaimAccepted
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-25" amountCorrect
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-26" claimNotPaidYet
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-27" claimMarkedAsPaid
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-28" isPaidCorrect
  && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-29" (isAfterClaimTimeToPay || isFidaCardOwner)
  where
    txInfo = scriptContextTxInfo sc

    ( TxOut insuranceAddress _ _ _,
      InsuranceInfo{iInfoClaim = Just (ClaimInfo {claimAmount, claimAccepted, claimId, claimDate}), ..}
     ) = unsafeReferenceOutput "ERROR-PIGGY-BANK-VALIDATOR-21" cs sc policyInfoTokenName

    paid = lovelaceValueOf $ mconcat
      [ value
      | TxOut address value (OutputDatum (Datum d)) _ <- txInfoOutputs txInfo
      , insuranceAddress == address
      , Just PolicyClaimPayment <- [fromBuiltinData d]
      ]

    isClaimAccepted = claimAccepted

    inputCollateral = case findOwnInput sc of
            Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
            Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-22"

    (outputCollateral, paidClaims) = case output cs sc fidaCardStatusTokenName of
            Just (TxOut _ value _ _, PBankFidaCard {pbfcPaidClaims=pbfcPaidClaims'}) -> (lovelaceValueOf value, pbfcPaidClaims')
            _ -> traceError "ERROR-PIGGY-BANK-VALIDATOR-23"

    amountCorrect = (claimAmount) >= iInfoFidaCardNumber * (inputCollateral - outputCollateral)

    claimNotPaidYet = not (claimId `elem` pbfcPaidClaims)

    claimMarkedAsPaid = claimId : pbfcPaidClaims == paidClaims

    isPaidCorrect = iInfoFidaCardNumber * paid >= claimAmount

    isFidaCardOwner = valueOf (valueSpent txInfo) cs (fidaCardTokenName n) == 1

    isAfterClaimTimeToPay = before (claimDate + fromMilliSeconds iInfoClaimTimeToPay) $ txInfoValidRange txInfo

mkPiggyBankValidator (InsuranceId cs) _ datum@(PBankPremium initAmount) ClaimPremiumOnCancel sc =
  traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-9" isPolicyCancelled
    && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-10" isSignedByPolicyHolder
    && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-11" isClaimedPremiumAmountValid
  where
    txInfo = scriptContextTxInfo sc

    (policyState, policyHolder, maybePolicyStartDate, paymentIntervals) =
      unsafeFromSingleton' "ERROR-PIGGY-BANK-VALIDATOR-12"
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

    unlockedPremium =
      case maybePolicyStartDate of
        Just start -> unlockedPremiumToClaim (txInfoValidRange txInfo) initAmount paymentIntervals start
        Nothing -> initAmount

    isClaimedPremiumAmountValid = lockedPremium >= initAmount - unlockedPremium

mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard _ _ _) UnlockCollateralOnCancel sc =
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
