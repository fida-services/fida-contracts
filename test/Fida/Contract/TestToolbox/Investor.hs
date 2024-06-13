{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.TestToolbox.Investor where

import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.TestToolbox.TypedValidators
import Plutus.Model
import Plutus.V2.Ledger.Api
import Control.Monad (sequence)
import Prelude

data FidaCard = FidaCard InsuranceId FidaCardId deriving (Show, Eq)

data PiggyBankInfo =
  PiggyBankInfo
    { piggyBank'id :: FidaCard
    , piggyBank'collateral :: Integer
    , piggyBank'collateralLocked :: Bool
    , piggyBank'initialCollateral :: Integer
    , piggyBank'premium :: InvestorPremium
    } deriving (Show, Eq)

data InvestorCollateral =
  InvestorCollateral
    { investorCollateral'locked :: Integer
    , investorCollateral'unlocked :: Integer
    } deriving (Show, Eq)

data InvestorPremium =
  InvestorPremium
    { investorPremium'locked :: Integer
    , investorPremium'available :: Integer
    } deriving (Show, Eq)

data InvestorPortfolio =
  InvestorPortfolio
    { investor'fidaCards :: [FidaCard]
    , investor'contribution :: [PiggyBankInfo]
    } deriving (Show, Eq)


findPiggyBankInfo :: FidaCard -> Run (Maybe PiggyBankInfo)
findPiggyBankInfo = undefined

withPiggyBankInfo ::
  FidaCard ->
  (PiggyBankInfo -> Run ()) ->
  Run ()
withPiggyBankInfo card = withMay err (findPiggyBankInfo card)
  where
    err = "Can't find piggy bank identified by " <> show card

getInvestorPortfolio :: PubKeyHash -> Run InvestorPortfolio
getInvestorPortfolio = do

  undefined
