{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.SystemIdMintingPolicy where

import Fida.Contract.Types
import Fida.Contract.Utils (mkUntypedMintingPolicy)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified

newtype SystemMagic = SystemMagic Integer
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
  mkUntypedMintingPolicy $
    mkSystemIdMintingPolicy
      (unsafeFromBuiltinData gov)
      (unsafeFromBuiltinData magic)

serialisableSystemIdMintingPolicy :: Script
serialisableSystemIdMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkSystemIdMintingPolicyUntyped||])
