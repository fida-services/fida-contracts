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
import qualified Plutus.V1.Ledger.Api as PlutusTx
import Plutus.V1.Ledger.Interval (before)
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
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx.Prelude

-- |
--
--  TODO: Add description
--
--  On chain errors:
--
--    ERR-FUND-0: Illegal action for Funding state
--
--    ERR-FUND-1: the tx is not signed by the policy authority
--
--    ERR-FUND-2: the output datum is not updated correctly
--
--    ERR-FUND-3: the number of sold fida cards is smaller than the number of fida cards
--
--    ERR-FUND-4: the output datum is not updated correctly
--
--    ERR-FUND-5: start date must be before valid range
--
--    ERR-FUND-6: tx is not signed by the policy holder or policy authority
{-# INLINEABLE lifecycleFundingStateValidator #-}
lifecycleFundingStateValidator ::
  InsuranceId ->
  InsurancePolicyDatum ->
  PolicyFundingRedeemer ->
  ScriptContext ->
  Bool
lifecycleFundingStateValidator (InsuranceId cs) d@InsuranceInfo {iInfoPolicyAuthority} PolicyFundingCancel sc =
  traceIfFalse "ERR-FUND-1" isSigned
    && traceIfFalse "ERR-FUND-2" hasCorrectOutput
 where
  isSigned = isSignedByTheAuthority sc iInfoPolicyAuthority

  outputDatum = untypedOutputDatum cs sc policyInfoTokenName

  hasCorrectOutput = outputDatum == untypedUpdatePolicyState d Cancelled
lifecycleFundingStateValidator (InsuranceId cs) (iinfo@InsuranceInfo {iInfoState = Funding, iInfoFidaCardNumber, iInfoPolicyHolder, iInfoPolicyAuthority}) (PolicyFundingFundingComplete startDate) sc =
  traceIfFalse "ERR-FUND-3" (fidaCardsSold >= iInfoFidaCardNumber)
    && traceIfFalse "ERR-FUND-4" hasCorrectOutput
    && traceIfFalse "ERR-FUND-5" isStartDateCorrect
    && traceIfFalse "ERR-FUND-6" (isSignedByPolicyHolder || isSignedByAuthority')
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
  
  isSignedByAuthority' = isSignedByTheAuthority sc iInfoPolicyAuthority
lifecycleFundingStateValidator _ _ _ _ = trace "ERR-FUND-0" False
