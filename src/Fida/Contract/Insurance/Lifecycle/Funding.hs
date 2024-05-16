{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Lifecycle.Funding (
    lifecycleFundingStateValidator,
) where

import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Fida.Contract.Insurance.Datum (
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum(..),
    updatePolicyState,
 )
import Fida.Contract.Insurance.Identifier (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (PolicyFundingRedeemer (PolicyFundingCancel, PolicyFundingExpire, PolicyFundingFund, PolicyFundingFundingComplete, PolicyFundingRetractFunding))
import Fida.Contract.Insurance.Tokens (policyInfoTokenName, fidaCardStatusTokenName)
import Fida.Contract.Utils (outputDatum, unsafeUntypedOutputDatum)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (
    Datum(Datum),
    OutputDatum(OutputDatum),
    ScriptContext (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    fromBuiltinData,
 )
import qualified Plutus.V2.Ledger.Api as PlutusTx
import PlutusTx.Prelude


{- |
    ERROR-FUNDING-VALIDATOR-0: the tx is not signed by the policy authority

    ERROR-FUNDING-VALIDATOR-1: the output datum is not updated correctly

    ERROR-FUNDING-VALIDATOR-2: the number of sold fida cards is smaller than the number of fida cards

    ERROR-FUNDING-VALIDATOR-3: the output datum is not updated correctly

    ERROR-FUNDING-VALIDATOR-4: the tx is not signed by the policy authority
-}

{-# INLINEABLE lifecycleFundingStateValidator #-}
lifecycleFundingStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    PolicyFundingRedeemer ->
    ScriptContext ->
    Bool
lifecycleFundingStateValidator (InsuranceId cs) InsuranceInfo{iInfoPolicyAuthority} PolicyFundingCancel sc =
        traceIfFalse "ERROR-FUNDING-VALIDATOR-0" isSigned
    where
        isSigned = case outputDatum cs sc policyInfoTokenName of
            Just (InsuranceInfo{iInfoState = Cancelled}) -> isSignedByTheAuthority sc iInfoPolicyAuthority
            _ -> False
lifecycleFundingStateValidator (InsuranceId cs) datum PolicyFundingFund sc =
        traceIfFalse "ERROR-FUNDING-VALIDATOR-1" correctOutputDatum
    where
        untypedOutputDatum = unsafeUntypedOutputDatum cs sc policyInfoTokenName
        correctOutputDatum =
            case outputDatum cs sc policyInfoTokenName of
                Just (InsuranceInfo{iInfoState = Funding}) ->
                    Just untypedOutputDatum == fmap PlutusTx.toBuiltinData (updatePolicyState datum Funding)
                _ -> False
lifecycleFundingStateValidator (InsuranceId cs) (d@InsuranceInfo{..}) PolicyFundingFundingComplete scriptContext =
    traceIfFalse "ERROR-FUNDING-VALIDATOR-2" (fidaCardsSold >= iInfoFidaCardNumber)
    && traceIfFalse "ERROR-FUNDING-VALIDATOR-3" correctOutput
    && traceIfFalse "ERROR-FUNDING-VALIDATOR-4" signedByAuthority
    where
        txInfo = scriptContextTxInfo scriptContext
        referenceInputs = txInfoReferenceInputs txInfo
        fidaCardsSold =
            length
            [ ()
            | TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) _) <- referenceInputs
            , valueOf v cs fidaCardStatusTokenName == 1
            , Just (PBankFidaCard {pbfcIsSold = True}) <- [fromBuiltinData d]
            ]
        outputDatum' = unsafeUntypedOutputDatum cs scriptContext policyInfoTokenName
        correctOutput = outputDatum' == PlutusTx.toBuiltinData (updatePolicyState d OnRisk)
        signedByAuthority = isSignedByTheAuthority scriptContext iInfoPolicyAuthority
lifecycleFundingStateValidator _ _ PolicyFundingExpire _ = True
lifecycleFundingStateValidator _ _ _ _ = False
