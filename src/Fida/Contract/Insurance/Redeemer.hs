{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Redeemer where

import qualified PlutusTx

data PolicyInitiatedRedemeer
    = PolicyInitiatedCancel
    | PolicyInitiatedPayPremium
    | PolicyInitiatedExpire

PlutusTx.makeIsDataIndexed
    ''PolicyInitiatedRedemeer
    [ ('PolicyInitiatedCancel, 0)
    , ('PolicyInitiatedPayPremium, 1)
    , ('PolicyInitiatedExpire, 2)
    ]

data PolicyFundingRedeemer
    = PolicyFundingCancel
    | PolicyFundingFundingComplete
    | PolicyFundingExpire

PlutusTx.makeIsDataIndexed
    ''PolicyFundingRedeemer
    [ ('PolicyFundingCancel, 0)
    , ('PolicyFundingFundingComplete, 1)
    , ('PolicyFundingExpire, 2)
    ]

data PolicyOnRiskRedeemer
    = PolicyOnRiskCreateClaim
    | PolicyOnRiskAcceptClaim

PlutusTx.makeIsDataIndexed
    ''PolicyOnRiskRedeemer
    [ ('PolicyOnRiskCreateClaim, 0)
    , ('PolicyOnRiskAcceptClaim, 1)
    ]

data InsurancePolicyRedeemer
    = PolicyInitiated PolicyInitiatedRedemeer
    | PolicyFunding PolicyFundingRedeemer
    | PolicyOnRisk PolicyOnRiskRedeemer

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyRedeemer
    [ ('PolicyInitiated, 0)
    , ('PolicyFunding, 1)
    , ('PolicyOnRisk, 2)
    ]

data PiggyBankRedeemer
    = BuyFidaCard
    | SellFidaCard
    | ClaimPremium
    | ClaimPremiumOnCancel
    | PayForClaimWithCollateral
    | UnlockCollateralOnCancel
    | ClaimCollateral

PlutusTx.makeIsDataIndexed
    ''PiggyBankRedeemer
    [ ('BuyFidaCard, 0)
    , ('SellFidaCard, 1)
    , ('ClaimPremium, 2)
    , ('PayForClaimWithCollateral, 3)
    , ('ClaimCollateral, 4)
    , ('ClaimPremiumOnCancel, 5)
    , ('UnlockCollateralOnCancel, 5)
    ]
