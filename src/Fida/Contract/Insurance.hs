{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance where

import Fida.Contract.Types ( PolicyAuthority )
import Plutus.V2.Ledger.Api
    ( fromCompiledCode,
      Address,
      Script,
      POSIXTime,
      ScriptContext,
      UnsafeFromData(unsafeFromBuiltinData) )
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude
import Fida.Contract.Insurance.Identifier (InsuranceId)

data InsurancePolicyRedeemer
  = BuyFidaCard Integer
  | PayPremium Integer
  | Activate

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyRedeemer
    [ ('BuyFidaCard, 0)
    , ('PayPremium, 1)
    , ('Activate, 2)
    ]

data InsurancePolicyState
  = Initialized
  | Funding
  | OnRisk
  | Cancelled
    deriving (Prelude.Show)

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyState
    [ ('Initialized, 0)
    , ('Funding, 1)
    , ('OnRisk, 2)
    , ('Cancelled, 3)
    ]

data InsurancePolicyDatum
  = InsuranceInfo
      { collateralAmount :: Integer
      , fidaCardValue :: Integer
      , premiumAmount :: Integer
      , policyHolder :: Address
      , policyAuthority :: PolicyAuthority
      , startDate :: Maybe POSIXTime
      , paymentIntervals :: Integer
      , contractState :: InsurancePolicyState
      }
  | FidaCardInfo
      { fidaCardValue :: Integer
      }
  | PremiumAmount
    deriving (Prelude.Show)

PlutusTx.makeIsDataIndexed ''InsurancePolicyDatum
  [ ('InsuranceInfo, 0)
  , ('FidaCardInfo, 1)
  , ('PremiumAmount, 2)
  ]

{-# INLINEABLE mkInsurancePolicyValidator #-}
mkInsurancePolicyValidator ::
    InsuranceId ->
    InsurancePolicyDatum ->
    InsurancePolicyRedeemer ->
    ScriptContext ->
    Bool
mkInsurancePolicyValidator _ _ _ _ = True

{-# INLINEABLE mkInsurancePolicyValidatorUntyped #-}
mkInsurancePolicyValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkInsurancePolicyValidatorUntyped insuranceId datum redeemer sc =
    check $
        mkInsurancePolicyValidator
            (unsafeFromBuiltinData insuranceId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData sc)

serialisableInsurancePolicyValidator :: Script
serialisableInsurancePolicyValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkInsurancePolicyValidatorUntyped||])
