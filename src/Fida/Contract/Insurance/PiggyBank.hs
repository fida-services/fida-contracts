{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.PiggyBank (
    serialisablePiggyBankValidator,
) where

import Fida.Contract.Insurance.Datum (PiggyBankDatum)
import Fida.Contract.Insurance.Identifier (InsuranceId)
import Fida.Contract.Insurance.Redeemer (PiggyBankRedeemer)
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude

newtype FidaCardId = FidaCardId Integer
    deriving newtype (ToData, FromData, UnsafeFromData)

{-# INLINEABLE mkPiggyBankValidator #-}
mkPiggyBankValidator ::
    InsuranceId ->
    FidaCardId ->
    PiggyBankDatum ->
    PiggyBankRedeemer ->
    ScriptContext ->
    Bool
mkPiggyBankValidator _ _ _ _ _ = True

{-# INLINEABLE mkPiggyBankValidatorUntyped #-}
mkPiggyBankValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkPiggyBankValidatorUntyped insuranceId fidaCardId datum redeemer sc =
    check $
        mkPiggyBankValidator
            (unsafeFromBuiltinData insuranceId)
            (unsafeFromBuiltinData fidaCardId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData sc)

serialisablePiggyBankValidator :: Script
serialisablePiggyBankValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkPiggyBankValidatorUntyped||])
