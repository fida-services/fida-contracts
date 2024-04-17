{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.FidaInvestorContract where

import Fida.Contract.Types
import Plutus.V2.Ledger.Api
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

{-# INLINEABLE mkInvestorContractValidator #-}
mkInvestorContractValidator ::
    PubKeyHash ->
    SystemId ->
    InvestorDatum ->
    InvestorRedeemer ->
    ScriptContext ->
    Bool
mkInvestorContractValidator _ _ _ _ _ = True

{-# INLINEABLE mkInvestorContractValidatorUntyped #-}
mkInvestorContractValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkInvestorContractValidatorUntyped pkh systemId datum redeemer scriptContext =
    check $
        mkInvestorContractValidator
            (unsafeFromBuiltinData pkh)
            (unsafeFromBuiltinData systemId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData scriptContext)

serialisableInvestorContractValidator :: Script
serialisableInvestorContractValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkInvestorContractValidatorUntyped||])
