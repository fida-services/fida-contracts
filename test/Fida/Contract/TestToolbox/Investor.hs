{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.TestToolbox.Investor where

import Control.Monad (join, filterM)
import Data.Maybe (maybeToList)
import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.TestToolbox.TypedValidators
import Plutus.Model
import Plutus.V2.Ledger.Api
import Plutus.V1.Ledger.Value (flattenValue)
import Control.Monad (sequence)
import qualified Data.ByteString as BS
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
getInvestorPortfolio pkh = do
  utxos <- utxoAt pkh
  let fidaCardCandidates = getFidaCards utxos
  investor'fidaCards <- filterM isFidaCard fidaCardCandidates
  investor'contribution <- join <$> traverse getPiggyBankInfo investor'fidaCards
  return InvestorPortfolio {..}
  where
    getPiggyBankInfo = fmap maybeToList . findPiggyBankInfo

    getFidaCards utxos =
      foldl (\acc (_, txOut) -> fidaCardsFromTxOut txOut <> acc) [] utxos

    isFidaCard :: FidaCard -> Run Bool
    isFidaCard (FidaCard iid fcid) = do
      boxes <- boxAt $ piggyBank iid fcid
      return $ not $
        null [ ()
             | box <- boxes
             , piggyBankInfoBox iid fcid box
             ]

fidaCardsFromTxOut :: TxOut -> [FidaCard]
fidaCardsFromTxOut (TxOut _ value _ _) =
  [ FidaCard (InsuranceId cs) (FidaCardId tn)
  | (cs, TokenName tn, n) <- flattenValue value
  , n == 1
  , BS.take 5 (fromBuiltin tn) == "CARD-"
  ]
