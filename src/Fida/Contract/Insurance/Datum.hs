{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Datum (
    InsurancePolicyState (..),
    InsurancePolicyDatum (..),
    PiggyBankDatum (..),
) where

import Fida.Contract.Insurance.Authority (InsuranceAuthority)
import Plutus.V2.Ledger.Api (Address, POSIXTime)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as HPrelude

data InsurancePolicyState
    = Initiated
    | Funding
    | OnRisk
    | Cancelled
    deriving (HPrelude.Show, HPrelude.Eq)

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyState
    [ ('Initiated, 0)
    , ('Funding, 1)
    , ('OnRisk, 2)
    , ('Cancelled, 3)
    ]

data InsurancePolicyDatum
    = InsuranceInfo
        { iInfoCollateralAmount :: Integer
        , iInfoFidaCardValue :: Integer
        , iInfoPremiumAmount :: Integer
        , iInfoPolicyHolder :: Address
        , iInfoPolicyAuthority :: InsuranceAuthority
        , iInfoStartDate :: Maybe POSIXTime
        , iInfoPaymentIntervals :: Integer
        , iInfoState :: InsurancePolicyState
        }
    | FidaCardInfo
        { fidaCardValue :: Integer
        }
    | PremiumAmount
    deriving (HPrelude.Show)

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyDatum
    [ ('InsuranceInfo, 0)
    , ('FidaCardInfo, 1)
    , ('PremiumAmount, 2)
    ]

data PiggyBankDatum
    = Collateral
    | Premium
    | FidaCard
    deriving (HPrelude.Show)

PlutusTx.makeIsDataIndexed
    ''PiggyBankDatum
    [ ('Collateral, 0)
    , ('Premium, 1)
    , ('FidaCard, 2)
    ]
