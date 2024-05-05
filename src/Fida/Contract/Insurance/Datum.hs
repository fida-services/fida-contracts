{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Datum
  ( InsurancePolicyState (..)
  , InsurancePolicyDatum (..)
  ) where

import Fida.Contract.Insurance.Authority (InsuranceAuthority)
import Plutus.V2.Ledger.Api ( Address, POSIXTime )
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
        { collateralAmount :: Integer
        , fidaCardValue :: Integer
        , premiumAmount :: Integer
        , policyHolder :: Address
        , policyAuthority :: InsuranceAuthority
        , startDate :: Maybe POSIXTime
        , paymentIntervals :: Integer
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
