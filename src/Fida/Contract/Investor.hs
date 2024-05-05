{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Investor (
    serialisableInvestorValidator,
) where

import Fida.Contract.SystemId (SystemId)
import Plutus.V2.Ledger.Api (
    BuiltinData,
    PubKeyHash,
    Script,
    ScriptContext,
    UnsafeFromData (unsafeFromBuiltinData),
    fromCompiledCode,
 )
import qualified PlutusTx
import PlutusTx.Prelude

type InvestorDatum = ()

data InvestorRedeemer
    = ProcessClaim
    | Withdraw

PlutusTx.makeIsDataIndexed
    ''InvestorRedeemer
    [ ('ProcessClaim, 0)
    , ('Withdraw, 1)
    ]

{-# INLINEABLE mkInvestorValidator #-}
mkInvestorValidator ::
    PubKeyHash ->
    SystemId ->
    InvestorDatum ->
    InvestorRedeemer ->
    ScriptContext ->
    Bool
mkInvestorValidator _ _ _ _ _ = True

{-# INLINEABLE mkInvestorValidatorUntyped #-}
mkInvestorValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkInvestorValidatorUntyped pkh systemId datum redeemer scriptContext =
    check $
        mkInvestorValidator
            (unsafeFromBuiltinData pkh)
            (unsafeFromBuiltinData systemId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData scriptContext)

serialisableInvestorValidator :: Script
serialisableInvestorValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkInvestorValidatorUntyped||])
