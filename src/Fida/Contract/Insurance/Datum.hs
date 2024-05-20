{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Datum (
    InsurancePolicyState (..),
    InsurancePolicyDatum (..),
    PiggyBankDatum (..),
    updatePolicyState,
    unlockedPremiumToClaim,
) where

import Fida.Contract.Insurance.Authority (InsuranceAuthority)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol, POSIXTime, TokenName, PubKeyHash)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as HPrelude
import Plutus.V1.Ledger.Api (POSIXTimeRange)
import Plutus.V1.Ledger.Time (DiffMilliSeconds, fromMilliSeconds)
import qualified Plutus.V1.Ledger.Interval as Interval

type PremiumAmount = Integer

data InsurancePolicyState
    = Initiated
    | Funding
    | OnRisk
    | Cancelled
    deriving (HPrelude.Show, HPrelude.Eq)

instance Eq InsurancePolicyState where
    {-# INLINEABLE (==) #-}
    Initiated == Initiated = True
    Funding == Funding = True
    OnRisk == OnRisk = True
    Cancelled == Cancelled = True
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''InsurancePolicyState
    [ ('Initiated, 0)
    , ('Funding, 1)
    , ('OnRisk, 2)
    , ('Cancelled, 3)
    ]

data InstallmentsInfo =
  InstallmentsInfo
    [DiffMilliSeconds] -- payment intervals
    PremiumAmount      -- how much to pay per installment (for one fida card)
  deriving (HPrelude.Show)

PlutusTx.makeIsDataIndexed
    ''InstallmentsInfo
    [ ('InstallmentsInfo , 0)
    ]


data InsurancePolicyDatum
    = InsuranceInfo
        { iInfoCollateralAmount :: Integer
        , iInfoFidaCardValue :: Integer
        , iInfoPremiumAmount :: Integer
        , iInfoPolicyHolder :: PubKeyHash
        , iInfoPolicyAuthority :: InsuranceAuthority
        , iInfoStartDate :: Maybe POSIXTime
        , iInfoInstallments :: InstallmentsInfo
        , iInfoState :: InsurancePolicyState
        , iInfoFidaCardNumber :: Integer
        , iInfoFidaCardPurchaseProofCurrencySymbol :: CurrencySymbol
        , iInfoFidaCardPurchaseProofTokenName :: TokenName
        }
    | FidaCardInfo
        { fidaCardValue :: Integer
        }
    | PremiumPaymentInfo
        { -- | in lovelace
          ppInfoPremiumAmountPerPiggyBank :: Integer
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
    | PBankPremium PremiumAmount
    | PBankFidaCard { pbfcIsSold :: Bool, pbfcFidaCardValue :: Integer }
    deriving (HPrelude.Show)

PlutusTx.makeIsDataIndexed
    ''PiggyBankDatum
    [ ('PBankCollateral, 0)
    , ('PBankPremium, 1)
    , ('PBankFidaCard, 2)
    ]

{-# INLINEABLE unlockedPremiumToClaim #-}
unlockedPremiumToClaim ::
  POSIXTimeRange ->   -- current time
  PremiumAmount ->    -- locked premium amount (initial amount)
  InstallmentsInfo ->  -- payment intervals
  POSIXTime ->        -- insurance policy start date
  PremiumAmount
unlockedPremiumToClaim range locked (InstallmentsInfo intervals dpa) start = go 0 start intervals
  where
    go _ _ [] = locked
    go unlocked date (dt:rest) =
      let nextDate = fromMilliSeconds dt + date
      in
        if (Interval.before nextDate range) then go (unlocked + dpa) nextDate rest
        else unlocked
