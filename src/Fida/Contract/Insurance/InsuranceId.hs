{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.InsuranceId (
    mkInsuranceIdMintingPolicy,
    serialisableInsuranceIdMintingPolicy,
    insuranceIdMintingPolicy,
    InsuranceId (..),
) where

import Fida.Contract.Utils (wrapPolicy)
import Plutus.V2.Ledger.Api (
    CurrencySymbol,
    FromData,
    MintingPolicy,
    Script,
    ScriptContext (..),
    ScriptPurpose (..),
    ToData,
    TxInInfo (..),
    TxInfo (..),
    TxOutRef,
    UnsafeFromData (unsafeFromBuiltinData),
    fromCompiledCode,
    mkMintingPolicyScript,
 )
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

-- | Unique Fida Insurance identifier
newtype InsuranceId = InsuranceId CurrencySymbol
    deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)

PlutusTx.makeLift ''InsuranceId

{-# INLINEABLE mkInsuranceIdMintingPolicy #-}
mkInsuranceIdMintingPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkInsuranceIdMintingPolicy ref _ (ScriptContext txInfo (Minting _)) =
    traceIfFalse "ERROR-INSURANCE-ID-MINTING-POLICY-0" isTxOutRefUsed
  where
    -- Ensure that the TxOutRef is used by the transaction.
    -- This parameter ensures the uniqueness of the currency symbol.
    isTxOutRefUsed =
        ref `elem` map txInInfoOutRef (txInfoInputs txInfo)
mkInsuranceIdMintingPolicy _ _ _ =
    trace "ERROR-INSURANCE-ID-MINTING-POLICY-1" False

{-# INLINEABLE mkInsuranceIdMintingPolicyUntyped #-}
mkInsuranceIdMintingPolicyUntyped ::
    -- | TxOutRef
    BuiltinData ->
    -- | Redeemer
    BuiltinData ->
    -- | ScriptContext
    BuiltinData ->
    ()
mkInsuranceIdMintingPolicyUntyped ref =
    wrapPolicy $
        mkInsuranceIdMintingPolicy
            (unsafeFromBuiltinData ref)

serialisableInsuranceIdMintingPolicy :: Script
serialisableInsuranceIdMintingPolicy =
    fromCompiledCode $$(PlutusTx.compile [||mkInsuranceIdMintingPolicyUntyped||])

insuranceIdMintingPolicy :: TxOutRef -> MintingPolicy
insuranceIdMintingPolicy oref =
    mkMintingPolicyScript $
        $$(PlutusTx.compile [||wrappedPolicy||])
            `PlutusTx.applyCode` PlutusTx.liftCode oref
  where
    wrappedPolicy = wrapPolicy . mkInsuranceIdMintingPolicy
