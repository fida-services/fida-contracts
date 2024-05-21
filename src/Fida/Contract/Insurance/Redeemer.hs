{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Redeemer where

import Fida.Contract.Insurance.Datum (ClaimInfo)
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
    = PolicyOnRiskCreateClaim ClaimInfo
    | PolicyOnRiskCancelClaim
    | PolicyOnRiskAcceptClaim
    | PolicyOnRiskFinalizeClaim
    | PolicyOnRiskExpireClaim
    | PolicyOnRiskFailClaim
    | PolicyOnRiskCancel
    | PolicyOnRiskExpire

PlutusTx.makeIsDataIndexed
    ''PolicyOnRiskRedeemer
    [ ('PolicyOnRiskCreateClaim, 0)
    , ('PolicyOnRiskCancelClaim, 1)
    , ('PolicyOnRiskAcceptClaim, 2)
    , ('PolicyOnRiskFinalizeClaim, 3)
    , ('PolicyOnRiskExpireClaim, 4)
    , ('PolicyOnRiskFailClaim, 5)
    , ('PolicyOnRiskCancel, 6)
    , ('PolicyOnRiskExpire, 7)
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
    | PayForClaimWithoutOwnFunds
    | UnlockCollateralOnCancel
    | ClaimCollateral

PlutusTx.makeIsDataIndexed
    ''PiggyBankRedeemer
    [ ('BuyFidaCard, 0)
    , ('SellFidaCard, 1)
    , ('ClaimPremium, 2)
    , ('PayForClaimWithCollateral, 3)
    , ('PayForClaimWithoutOwnFunds, 4)
    , ('ClaimCollateral, 5)
    , ('ClaimPremiumOnCancel, 6)
    , ('UnlockCollateralOnCancel, 7)
    ]
