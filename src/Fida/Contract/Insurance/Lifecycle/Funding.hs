{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Lifecycle.Funding
  ( lifecycleFundingStateValidator,
  )
where

import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Fida.Contract.Insurance.Datum
  ( FidaCardId (..),
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
    untypedUpdatePolicyState,
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PolicyFundingRedeemer (PolicyFundingCancel, PolicyFundingFundingComplete))
import Fida.Contract.Insurance.Tokens (fidaCardStatusTokenName, policyInfoTokenName)
import Fida.Contract.Utils (untypedOutputDatum)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
  ( Datum (Datum),
    OutputDatum (OutputDatum),
    ScriptContext (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    fromBuiltinData,
  )
import PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Api as PlutusTx
import Plutus.V1.Ledger.Interval (before)
import Plutus.V2.Ledger.Contexts (txSignedBy)

-- |
--
--  TODO: Add description
--
--  On chain errors:
--
--    ERROR-FUNDING-VALIDATOR-0: Illegal action for Funding state
--
--    ERROR-FUNDING-VALIDATOR-1: the tx is not signed by the policy authority
--
--    ERROR-FUNDING-VALIDATOR-2: the output datum is not updated correctly
--
--    ERROR-FUNDING-VALIDATOR-3: the number of sold fida cards is smaller than the number of fida cards
--
--    ERROR-FUNDING-VALIDATOR-4: the output datum is not updated correctly
--
--    ERROR-FUNDING-VALIDATOR-5: start date must be before valid range
--
--    ERROR-FUNDING-VALIDATOR-6: tx is not signed by the policy holder
--
{-# INLINEABLE lifecycleFundingStateValidator #-}
lifecycleFundingStateValidator ::
  InsuranceId ->
  InsurancePolicyDatum ->
  PolicyFundingRedeemer ->
  ScriptContext ->
  Bool
lifecycleFundingStateValidator (InsuranceId cs) d@InsuranceInfo {iInfoPolicyAuthority} PolicyFundingCancel sc =
  traceIfFalse "ERROR-FUNDING-VALIDATOR-1" isSigned
    && traceIfFalse "ERROR-FUNDING-VALIDATOR-2" hasCorrectOutput
 where
  isSigned = isSignedByTheAuthority sc iInfoPolicyAuthority

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  hasCorrectOutput = outputDatum == untypedUpdatePolicyState d Cancelled

lifecycleFundingStateValidator (InsuranceId cs) (iinfo@InsuranceInfo {iInfoState = Funding, iInfoFidaCardNumber, iInfoPolicyHolder}) (PolicyFundingFundingComplete startDate) sc =
  traceIfFalse "ERROR-FUNDING-VALIDATOR-3" (fidaCardsSold >= iInfoFidaCardNumber)
    && traceIfFalse "ERROR-FUNDING-VALIDATOR-4" hasCorrectOutput
    && traceIfFalse "ERROR-FUNDING-VALIDATOR-5" isStartDateCorrect
    && traceIfFalse "ERROR-FUNDING-VALIDATOR-6" isSignedByPolicyHolder
 where
  txInfo = scriptContextTxInfo sc

  referenceInputs = txInfoReferenceInputs txInfo

  fidaCardsSold =
    length . nub $
      [ cid
      | TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) _) <- referenceInputs
      , valueOf v cs fidaCardStatusTokenName == 1
      , Just (PBankFidaCard {pbfcIsSold = True, pbfcFidaCardId = FidaCardId cid}) <- [fromBuiltinData d]
      ]

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  updatedDatum = PlutusTx.toBuiltinData $ iinfo {iInfoState = OnRisk, iInfoStartDate = Just startDate}

  hasCorrectOutput = outputDatum == Just updatedDatum

  isStartDateCorrect = startDate `before` txInfoValidRange txInfo

  isSignedByPolicyHolder = txSignedBy txInfo iInfoPolicyHolder

lifecycleFundingStateValidator _ _ _ _ = trace "ERROR-FUNDING-VALIDATOR-0" False
