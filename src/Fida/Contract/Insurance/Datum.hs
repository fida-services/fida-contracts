{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Datum (
    InsurancePolicyState (..),
    InsurancePolicyDatum (..),
    PiggyBankDatum (..),
    updatePolicyState,
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

instance Eq InsurancePolicyState where
  {-# INLINABLE (==) #-}
  Initiated == Initiated = True
  Funding   == Funding   = True
  OnRisk    == OnRisk    = True
  Cancelled == Cancelled = True
  _         == _         = False

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
    | PremiumPaymentInfo
        { ppInfoPremiumAmountPerPiggyBank :: Integer -- ^ in lovelace 
        , ppInfoPiggyBanks :: [Address]
        }
    deriving (HPrelude.Show)

{-# INLINEABLE updatePolicyState #-}
updatePolicyState :: InsurancePolicyDatum -> InsurancePolicyState -> Maybe InsurancePolicyDatum
updatePolicyState InsuranceInfo{..} state = Just $ InsuranceInfo{iInfoState = state, ..}
updatePolicyState _ _ = Nothing

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyDatum
    [ ('InsuranceInfo, 0)
    , ('FidaCardInfo, 1)
    , ('PremiumPaymentInfo, 2)
    ]

data PiggyBankDatum
    = PBankCollateral
    | PBankPremium
    | PBankFidaCard
    deriving (HPrelude.Show)

PlutusTx.makeIsDataIndexed
    ''PiggyBankDatum
    [ ('PBankCollateral, 0)
    , ('PBankPremium, 1)
    , ('PBankFidaCard, 2)
    ]
