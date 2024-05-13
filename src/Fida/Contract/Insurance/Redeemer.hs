{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Redeemer where

import qualified PlutusTx

data InitStRedeemer
    = InitStCancell
    | InitStPayPremium

data InsurancePolicyRedeemer
    = FinaliseFunding
    | InitSt InitStRedeemer
    | Activate

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyRedeemer
    [ ('FinaliseFunding, 0)
    , ('InitSt, 1)
    , ('Activate, 2)
    ]

PlutusTx.makeIsDataIndexed
    ''InitStRedeemer
    [ ('InitStCancell, 0)
    , ('InitStPayPremium, 1)
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
