{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator) where

import Fida.Contract.Insurance.Authority (isSignedByAuth)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..), PiggyBankDatum (..), updatePolicyState)
import Fida.Contract.Insurance.Identifier (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (InitStRedeemer (..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName, policyPaymentTokenName)
import Fida.Contract.Utils (lovelaceOf, traceIfNotSingleton)
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
 Funding. The input related to the payment information must be consumed.

 On chain errors:

  ERROR-INITST-VALIDATOR-0: Illegal action for Initiated state

  ERROR-INITST-VALIDATOR-1: Transaction not signed by policy authority

  ERROR-INITST-VALIDATOR-2: Policy state not changed to Cancelled

  ERROR-INITST-VALIDATOR-3: Payments not properly distributed

  ERROR-INITST-VALIDATOR-4: Policy state not changed to Funding

  ERROR-INITST-VALIDATOR-5: There should be no new outputs on insuarance policy script address

  ERROR-INITST-VALIDATOR-6: Payment info not marked by NFT
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
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-4" changedToFunding
        && traceIfFalse "ERROR-INITST-VALIDATOR-5" noContinuingOutputs
        && traceIfNotSingleton "ERROR-INITST-VALIDATOR-6" consumedInputIsValid
  where
    txInfo = scriptContextTxInfo sc

    refInputs = txInInfoResolved <$> txInfoReferenceInputs txInfo

    noContinuingOutputs :: Bool
    noContinuingOutputs = null $ getContinuingOutputs sc

    consumedInputIsValid :: [Bool]
    consumedInputIsValid =
        [ True
        | Just (TxInInfo _ (TxOut _ value _ _)) <- [findOwnInput sc]
        , valueOf value cs policyPaymentTokenName == 1
        ]

    changedToFunding :: [Bool]
    changedToFunding =
        [ True
        | TxOut _ value (OutputDatum (Datum datum)) _ <- refInputs
        , valueOf value cs policyInfoTokenName == 1
        , Just (InsuranceInfo{iInfoState}) <- [PlutusTx.fromBuiltinData datum]
        , iInfoState == Funding
        ]

    payments :: [Address]
    payments =
        [ address
        | TxOut address value (OutputDatum (Datum datum)) _ <- txInfoOutputs txInfo
        , Just PBankPremium <- [PlutusTx.fromBuiltinData datum]
        , elem address ppInfoPiggyBanks
        , lovelaceOf value >= ppInfoPremiumAmountPerPiggyBank
        ]
lifecycleInitiatedStateValidator _ _ _ _ =
    trace "ERROR-INITST-VALIDATOR-0" False
