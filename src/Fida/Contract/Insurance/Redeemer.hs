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
    | PolicyFundingFund
    | PolicyFundingFundingComplete
    | PolicyFundingRetractFunding
    | PolicyFundingExpire

PlutusTx.makeIsDataIndexed
    ''PolicyFundingRedeemer
    [ ('PolicyFundingCancel, 0)
    , ('PolicyFundingFund, 1)
    , ('PolicyFundingFundingComplete, 2)
    , ('PolicyFundingRetractFunding, 3)
    , ('PolicyFundingExpire, 4)
    ]

data InsurancePolicyRedeemer
    = PolicyInitiated PolicyInitiatedRedemeer
    | PolicyFunding PolicyFundingRedeemer

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyRedeemer
    [ ('PolicyInitiated, 0)
    , ('PolicyFunding, 1)
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
