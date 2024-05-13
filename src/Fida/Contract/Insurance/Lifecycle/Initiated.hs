{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator) where

import Fida.Contract.Insurance.Authority (isSignedByAuth)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..), PiggyBankDatum (..), updatePolicyState)
import Fida.Contract.Insurance.Identifier (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (InitStRedeemer (..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName, policyPaymentTokenName)
import Fida.Contract.Utils (lovelaceValueOf, traceIfNotSingleton)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Api as PlutusTx
import Plutus.V2.Ledger.Contexts
import PlutusTx.Prelude

{- |

 At the script address, you are expected to have at least two UTxOs with the
 following specifications:
 - The first UTxO with datum containing information about insurance
   (InsuranceInfo) in the Initiated state and marked with policy info NFT
   (InsuranceId, POLICY-INFO).
 - The second one with datum (PremiumPaymentInfo) indicating how much the
   insurance owner (policy holder) should pay for the insurance. The utxo
   must be marked by payment info NFT (InsuranceId, POLICY-PAYMENT)

 The following actions are possible to perform:
  - Cancel insurance (InitStCancell)
  - Pay for insurance (InitStPayPremium)

 The cancellation action must be signed by the policy authority, respecting
 InsuranceAuthority strategy, and the contract state must transition to
 Cancelled.

 The payment action should evenly distribute payments to all piggy banks
 associated with the policy and transition the insurance policy state to
 Funding. The input related to the payment information could be consumed.

 On chain errors:

  ERROR-INITST-VALIDATOR-0: Illegal action for Initiated state

  ERROR-INITST-VALIDATOR-1: Transaction not signed by policy authority

  ERROR-INITST-VALIDATOR-2: Insurance policy state not changed to Cancelled

  ERROR-INITST-VALIDATOR-3: Payments not properly distributed

  ERROR-INITST-VALIDATOR-4: UTxO with policy datum was not spent

  ERROR-INITST-VALIDATOR-5: Payment info not marked by NFT

  ERROR-INITST-VALIDATOR-6: Insurance policy state not changed to Funding

  ERROR-INITST-VALIDATOR-7: UTxO with premium payment was not spent
-}
{-# INLINEABLE lifecycleInitiatedStateValidator #-}
lifecycleInitiatedStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InitStRedeemer ->
    ScriptContext ->
    Bool
lifecycleInitiatedStateValidator (InsuranceId cs) datum@(InsuranceInfo{iInfoState = Initiated}) InitStCancell sc =
    traceIfFalse "ERROR-INITST-VALIDATOR-1" isSigned
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-2" verifyOut
  where
    isSigned = isSignedByAuth sc $ iInfoPolicyAuthority datum
    verifyOut :: [Bool]
    verifyOut =
        [ True
        | TxOut _ value (OutputDatum datum') _ <- getContinuingOutputs sc
        , valueOf value cs policyInfoTokenName == 1
        , Just (getDatum datum') == fmap PlutusTx.toBuiltinData (updatePolicyState datum Cancelled)
        ]
lifecycleInitiatedStateValidator (InsuranceId cs) PremiumPaymentInfo{..} InitStPayPremium sc =
    traceIfFalse "ERROR-INITST-VALIDATOR-3" (length (nub payments) == length ppInfoPiggyBanks)
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-4" isPolicyInfoSpent
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-5" consumedInputIsValid
  where
    txInfo = scriptContextTxInfo sc

    consumedInputIsValid :: [Bool]
    consumedInputIsValid =
        [ True
        | Just (TxInInfo _ (TxOut _ value _ _)) <- [findOwnInput sc]
        , valueOf value cs policyPaymentTokenName == 1
        ]

    isPolicyInfoSpent :: [Bool]
    isPolicyInfoSpent =
        [ True
        | TxOut _ value _ _ <- getContinuingOutputs sc
        , valueOf value cs policyInfoTokenName == 1
        ]

    payments :: [Address]
    payments =
        [ address
        | TxOut address value (OutputDatum (Datum datum)) _ <- txInfoOutputs txInfo
        , Just PBankPremium <- [PlutusTx.fromBuiltinData datum]
        , elem address ppInfoPiggyBanks
        , lovelaceValueOf value >= ppInfoPremiumAmountPerPiggyBank
        ]
lifecycleInitiatedStateValidator (InsuranceId cs) InsuranceInfo{} InitStPayPremium sc =
    traceIfNotSingleton  "ERROR-INITST-VALIDATOR-6" verifyOut
      && traceIfNotSingleton  "ERROR-INITST-VALIDATOR-7" isPremiumPaymentSpent
  where
    txInfo = scriptContextTxInfo sc

    verifyOut :: [Bool]
    verifyOut =
        [ True
        | TxOut _ value (OutputDatum (Datum datum)) _ <- getContinuingOutputs sc
        , valueOf value cs policyInfoTokenName == 1
        , Just (InsuranceInfo{iInfoState}) <- [PlutusTx.fromBuiltinData datum]
        , iInfoState == Funding
        ]
    isPremiumPaymentSpent :: [Bool]
    isPremiumPaymentSpent =
      [ True
      | TxInInfo _ (TxOut _ value _ _) <- txInfoInputs txInfo
      , valueOf value cs policyPaymentTokenName == 1
      ]

 

lifecycleInitiatedStateValidator _ _ _ _ =
    trace "ERROR-INITST-VALIDATOR-0" False
