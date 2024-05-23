{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Redeemer where

import Fida.Contract.Insurance.Datum (ClaimInfo)
import qualified PlutusTx

data PolicyInitiatedRedemeer
    = PolicyInitiatedCancel
    | PolicyInitiatedPayPremium

PlutusTx.makeIsDataIndexed
    ''PolicyInitiatedRedemeer
    [ ('PolicyInitiatedCancel, 0)
    , ('PolicyInitiatedPayPremium, 1)
    ]

data PolicyFundingRedeemer
    = PolicyFundingCancel
    | PolicyFundingFundingComplete

PlutusTx.makeIsDataIndexed
    ''PolicyFundingRedeemer
    [ ('PolicyFundingCancel, 0)
    , ('PolicyFundingFundingComplete, 1)
    ]

data PolicyOnRiskRedeemer
    = PolicyOnRiskCreateClaim ClaimInfo
    | PolicyOnRiskCancelClaim
    | PolicyOnRiskAcceptClaim
    | PolicyOnRiskFinalizeClaim
    | PolicyOnRiskExpireClaim
    | PolicyOnRiskFailClaim
    | PolicyOnRiskCancel
    | PolicyOnRiskClaimPayment

PlutusTx.makeIsDataIndexed
    ''PolicyOnRiskRedeemer
    [ ('PolicyOnRiskCreateClaim, 0)
    , ('PolicyOnRiskCancelClaim, 1)
    , ('PolicyOnRiskAcceptClaim, 2)
    , ('PolicyOnRiskFinalizeClaim, 3)
    , ('PolicyOnRiskExpireClaim, 4)
    , ('PolicyOnRiskFailClaim, 5)
    , ('PolicyOnRiskCancel, 6)
    , ('PolicyOnRiskClaimPayment, 7)
    ]

data InsurancePolicyRedeemer
    = PolicyInitiated PolicyInitiatedRedemeer
    | PolicyFunding PolicyFundingRedeemer
    | PolicyOnRisk PolicyOnRiskRedeemer
    | PolicyExpire

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyRedeemer
    [ ('PolicyInitiated, 0)
    , ('PolicyFunding, 1)
    , ('PolicyOnRisk, 2)
    , ('PolicyExpire, 3)
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
