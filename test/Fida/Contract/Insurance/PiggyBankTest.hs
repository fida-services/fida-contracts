{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.PiggyBankTest (tests) where

import Fida.Contract.TestToolbox
  ( Run,
    Users (..),
    bad,
    fidaCardsFromInts,
    good,
    insurancePolicy,
    newSamplePolicy,
    runUpdatePolicyState,
    setupUsers,
  )
import Fida.Contract.TestToolbox.Action (buyFidaCards)
import Fida.Contract.TestToolbox.TypedValidators (PiggyBank)
import Plutus.Model
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unit tests for PiggyBank module"
    [ good "Buying Fida card works" testBuyFidaCard
    ]

testBuyFidaCard :: Run ()
testBuyFidaCard = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  buyFidaCards iid investor1 $ fidaCardsFromInts [1 .. 5]
