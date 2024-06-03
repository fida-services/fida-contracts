{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fida.Contract.TestToolbox.Users
  ( Users(..)
  , setupUsers
  ) where

import Control.Monad (replicateM)
import Plutus.Model (Run, newUser, adaValue, writeUserName)
import Plutus.V2.Ledger.Api (PubKeyHash)
import Prelude


data Users = Users
  { fidaSystem :: PubKeyHash
  , broker1 :: PubKeyHash
  , broker2 :: PubKeyHash
  , policyHolder :: PubKeyHash
  , investor1 :: PubKeyHash
  , investor2 :: PubKeyHash
  , investor3 :: PubKeyHash
  }
  deriving (Show)


setupUsers :: Run Users
setupUsers = do
  users <- replicateM 7 $ newUser $ adaValue 100_000_000
  let [fidaSystem, broker1, broker2, policyHolder, investor1, investor2, investor3] = users
  writeUserName fidaSystem "Fida System Wallet"
  writeUserName broker1 "Policy Broker 1"
  writeUserName broker2 "Policy Broker 2"
  writeUserName policyHolder "Policy Holder"
  writeUserName investor1 "Investor 1"
  writeUserName investor2 "Investor 2"
  writeUserName investor3 "Investor 3"
  return Users {..}
