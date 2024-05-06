{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Redeemer where

import qualified PlutusTx

data InsurancePolicyRedeemer
    = FinaliseFunding
    | PayPremium
    | Activate

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyRedeemer
    [ ('FinaliseFunding, 0)
    , ('PayPremium, 1)
    , ('Activate, 2)
    ]

data PiggyBankRedeemer
    = BuyFidaCard
    | ClaimPremium
    | UnlockCollateral
    | ClaimCollateral

PlutusTx.makeIsDataIndexed
    ''PiggyBankRedeemer
    [ ('BuyFidaCard, 0)
    , ('ClaimPremium, 1)
    , ('UnlockCollateral, 2)
    , ('ClaimCollateral, 3)
    ]
