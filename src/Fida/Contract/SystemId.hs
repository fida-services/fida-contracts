{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.SystemId (
    SystemId (..),
    serialisableSystemIdMintingPolicy,
) where

import Fida.Contract.Utils (wrapPolicy)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

newtype SystemGovernance = SystemGovernance PubKeyHash
    deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)

newtype SystemMagic = SystemMagic Integer
    deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)

newtype SystemId = SystemId CurrencySymbol
    deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)

{-# INLINEABLE mkSystemIdMintingPolicy #-}
mkSystemIdMintingPolicy :: SystemGovernance -> SystemMagic -> () -> ScriptContext -> Bool
mkSystemIdMintingPolicy (SystemGovernance pkh) _ _ (ScriptContext txInfo (Minting _)) =
    traceIfFalse "ERROR-SYSTEM-ID-MINTING-POLICY-0" isSignedBySystem
  where
    isSignedBySystem = txSignedBy txInfo pkh
mkSystemIdMintingPolicy _ _ _ _ =
    trace "ERROR-SYSTEM_ID-MINTING-POLICY-1" False

{-# INLINEABLE mkSystemIdMintingPolicyUntyped #-}
mkSystemIdMintingPolicyUntyped ::
    -- | SystemGovernance
    BuiltinData ->
    -- | SystemMagic
    BuiltinData ->
    -- | Redeemer
    BuiltinData ->
    -- | ScriptContext
    BuiltinData ->
    ()
mkSystemIdMintingPolicyUntyped gov magic =
    wrapPolicy $
        mkSystemIdMintingPolicy
            (unsafeFromBuiltinData gov)
            (unsafeFromBuiltinData magic)

serialisableSystemIdMintingPolicy :: Script
serialisableSystemIdMintingPolicy =
    fromCompiledCode $$(PlutusTx.compile [||mkSystemIdMintingPolicyUntyped||])
