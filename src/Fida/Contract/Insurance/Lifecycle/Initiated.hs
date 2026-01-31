{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Lifecycle.Initiated (lifecycleInitiatedStateValidator) where

import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Fida.Contract.Insurance.Datum (InsurancePolicyDatum (..), InsurancePolicyState (..), PiggyBankDatum (..), untypedUpdatePolicyState)
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PolicyInitiatedRedemeer (..))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName, policyPaymentTokenName)
import Fida.Contract.Utils (lovelaceValueOf, traceIfNotSingleton, untypedOutputDatum)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
  ( Address,
    Datum (..),
    OutputDatum (..),
    ScriptContext (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
  )
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import qualified PlutusTx
import PlutusTx.Prelude

-- |
--
-- At the script address, you are expected to have at least two UTxOs with the
-- following specifications:
-- - The first UTxO with datum containing information about insurance
--   (InsuranceInfo) in the Initiated state and marked with policy info NFT
--   (InsuranceId, POLICY-INFO).
-- - The second one with datum (PremiumPaymentInfo) indicating how much the
--   insurance owner (policy holder) should pay for the insurance. The utxo
--   must be marked by payment info NFT (InsuranceId, POLICY-PAYMENT)
--
-- The following actions are possible to perform:
--  - Cancel insurance (InitStCancell)
--  - Pay for insurance (InitStPayPremium)
--
-- The cancellation action must be signed by the policy authority, respecting
-- InsuranceAuthority strategy, and the contract state must transition to
-- Cancelled.
--
-- The payment action should evenly distribute payments to all piggy banks
-- associated with the policy and transition the insurance policy state to
-- Funding. The input related to the payment information could be consumed.
--
-- On chain errors:
--
--  ERR-INIT-0: Illegal action for Initiated state
--
--  ERR-INIT-1: Transaction not signed by policy authority
--
--  ERR-INIT-2: Insurance policy state not changed to Cancelled
--
--  ERR-INIT-3: Payments not properly distributed
--
--  ERR-INIT-4: UTxO with policy datum was not spent
--
--  ERR-INIT-5: Payment info not marked by NFT
--
--  ERR-INIT-6: Insurance policy state not changed to Funding
--
--  ERR-INIT-7: UTxO with premium payment was not spent
{-# INLINEABLE lifecycleInitiatedStateValidator #-}
lifecycleInitiatedStateValidator ::
  InsuranceId ->
  InsurancePolicyDatum ->
  PolicyInitiatedRedemeer ->
  ScriptContext ->
  Bool
lifecycleInitiatedStateValidator (InsuranceId cs) datum@(InsuranceInfo {iInfoState = Initiated}) PolicyInitiatedCancel sc =
  traceIfFalse "ERR-INIT-1" isSigned
    && traceIfFalse "ERR-INIT-2" hasCorrectOutput
 where
  isSigned = isSignedByTheAuthority sc $ iInfoPolicyAuthority datum

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  hasCorrectOutput = outputDatum == untypedUpdatePolicyState datum Cancelled
lifecycleInitiatedStateValidator (InsuranceId cs) PremiumPaymentInfo {..} PolicyInitiatedPayPremium sc =
  traceIfFalse "ERR-INIT-3" (length (nub payments) == length ppInfoPiggyBanks)
    && traceIfNotSingleton "ERR-INIT-4" isPolicyInfoSpent
 where
  txInfo = scriptContextTxInfo sc

  isPolicyInfoSpent =
    [ True
    | TxOut _ value _ _ <- getContinuingOutputs sc
    , valueOf value cs policyInfoTokenName == 1
    ]

  payments :: [Address]
  payments =
    [ address
    | TxOut address value (OutputDatum (Datum datum)) _ <- txInfoOutputs txInfo
    , Just (PBankPremium amount refund) <- [PlutusTx.fromBuiltinData datum]
    , elem address ppInfoPiggyBanks
    , refund == 0
    , let paid = lovelaceValueOf value
       in paid >= ppInfoPremiumAmountPerPiggyBank && paid == amount
    ]
lifecycleInitiatedStateValidator (InsuranceId cs) d@InsuranceInfo {} PolicyInitiatedPayPremium sc =
  traceIfFalse "ERR-INIT-6" hasCorrectOutput
    && traceIfNotSingleton "ERR-INIT-7" isPremiumPaymentSpent
 where
  txInfo = scriptContextTxInfo sc

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  hasCorrectOutput = outputDatum == untypedUpdatePolicyState d Funding

  isPremiumPaymentSpent :: [Bool]
  isPremiumPaymentSpent =
    [ True
    | TxInInfo _ (TxOut _ value _ _) <- txInfoInputs txInfo
    , valueOf value cs policyPaymentTokenName == 1
    ]
lifecycleInitiatedStateValidator _ _ _ _ =
  trace "ERR-INIT-0" False
