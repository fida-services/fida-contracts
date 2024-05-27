{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Lifecycle.Funding (
    lifecycleFundingStateValidator,
) where

import Fida.Contract.Insurance.Authority (isSignedByTheAuthority)
import Fida.Contract.Insurance.Datum (
    FidaCardId (..),
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
import Plutus.V2.Ledger.Api (
    Datum (Datum),
    OutputDatum (OutputDatum),
    ScriptContext (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    fromBuiltinData,
 )
import PlutusTx.Prelude

{- |

  TODO: Add description

  On chain errors:

    ERROR-FUNDING-VALIDATOR-0: Illegal action for Funding state

    ERROR-FUNDING-VALIDATOR-1: the tx is not signed by the policy authority

    ERROR-FUNDING-VALIDATOR-2: the output datum is not updated correctly

    ERROR-FUNDING-VALIDATOR-3: the number of sold fida cards is smaller than the number of fida cards

    ERROR-FUNDING-VALIDATOR-4: the output datum is not updated correctly
-}
{-# INLINEABLE lifecycleFundingStateValidator #-}
lifecycleFundingStateValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    PolicyFundingRedeemer ->
    ScriptContext ->
    Bool
lifecycleFundingStateValidator (InsuranceId cs) d@InsuranceInfo{iInfoPolicyAuthority} PolicyFundingCancel sc =
    traceIfFalse "ERROR-FUNDING-VALIDATOR-1" isSigned
        && traceIfFalse "ERROR-FUNDING-VALIDATOR-2" hasCorrectOutput
  where
    isSigned = isSignedByTheAuthority sc iInfoPolicyAuthority

    outputDatum = untypedOutputDatum cs sc policyInfoTokenName

    hasCorrectOutput = outputDatum == untypedUpdatePolicyState d Cancelled
lifecycleFundingStateValidator (InsuranceId cs) (iinfo@InsuranceInfo{iInfoState = Funding, iInfoFidaCardNumber}) PolicyFundingFundingComplete sc =
    traceIfFalse "ERROR-FUNDING-VALIDATOR-3" (fidaCardsSold >= iInfoFidaCardNumber)
        && traceIfFalse "ERROR-FUNDING-VALIDATOR-4" hasCorrectOutput
  where
    txInfo = scriptContextTxInfo sc

    referenceInputs = txInfoReferenceInputs txInfo

    fidaCardsSold =
        length . nub $
            [ cid
            | TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) _) <- referenceInputs
            , valueOf v cs fidaCardStatusTokenName == 1
            , Just (PBankFidaCard{pbfcIsSold = True, pbfcFidaCardId = FidaCardId cid}) <- [fromBuiltinData d]
            ]

    outputDatum = untypedOutputDatum cs sc policyInfoTokenName

    hasCorrectOutput = outputDatum == untypedUpdatePolicyState iinfo OnRisk
lifecycleFundingStateValidator _ _ _ _ = trace "ERROR-FUNDING-VALIDATOR-0" False
