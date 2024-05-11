{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Fida.Contract.Insurance.Lifecycle.Funding (
    lifecycleFundingStateValidator,
) where

import Fida.Contract.Insurance.Datum
    ( InsurancePolicyState(..),
      InsurancePolicyDatum(..),
      updatePolicyState)
import Fida.Contract.Insurance.Identifier (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PolicyFundingRedeemer (PolicyFundingCancel, PolicyFundingExpire, PolicyFundingFund, PolicyFundingFundingComplete, PolicyFundingRetractFunding))
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
    ( ScriptContext(..),
      TxInInfo(..),
      TxInfo(..),
      TxOut(..) )
import PlutusTx.Prelude
import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Fida.Contract.Utils (unsafeUntypedOutputDatum, outputDatum)
import Fida.Contract.Insurance.Tokens (policyInfoTokenName)
import qualified Plutus.V2.Ledger.Api as PlutusTx

{-# INLINEABLE lifecycleFundingStateValidator #-}
lifecycleFundingStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    PolicyFundingRedeemer ->
    ScriptContext ->
    Bool
lifecycleFundingStateValidator (InsuranceId cs) InsuranceInfo{iInfoPolicyAuthority} PolicyFundingCancel sc =
    case outputDatum cs sc policyInfoTokenName of
        Just (InsuranceInfo{iInfoState = Cancelled}) -> isSignedByTheAuthority sc iInfoPolicyAuthority
        _ -> False
lifecycleFundingStateValidator (InsuranceId cs) _ PolicyFundingFund sc =
    case outputDatum cs sc policyInfoTokenName  of
        Just (InsuranceInfo{iInfoState = Funding}) -> True
        _ -> False
lifecycleFundingStateValidator (InsuranceId cs) (d@InsuranceInfo{..}) PolicyFundingFundingComplete scriptContext =
    let txInfo = scriptContextTxInfo scriptContext
        referenceInputs = txInfoReferenceInputs txInfo
        totalReferenceValue = sum $ map (\(TxInInfo _ (TxOut _ v _ _)) -> valueOf v iInfoFidaCardPurchaseProofCurrencySymbol iInfoFidaCardPurchaseProofTokenName) referenceInputs
        outputDatum' = unsafeUntypedOutputDatum cs scriptContext policyInfoTokenName
        correctOutput = outputDatum' == PlutusTx.toBuiltinData (updatePolicyState d OnRisk)
        signedByAuthority = isSignedByTheAuthority scriptContext iInfoPolicyAuthority
     in totalReferenceValue >= iInfoFidaCardNumber && correctOutput && signedByAuthority
lifecycleFundingStateValidator _ _ PolicyFundingRetractFunding _ = True
lifecycleFundingStateValidator _ _ PolicyFundingExpire _ = True
lifecycleFundingStateValidator _ _ _ _ = False
