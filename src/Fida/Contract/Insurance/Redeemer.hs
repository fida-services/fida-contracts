{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Redeemer where

import qualified PlutusTx
import PlutusTx.Prelude

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
