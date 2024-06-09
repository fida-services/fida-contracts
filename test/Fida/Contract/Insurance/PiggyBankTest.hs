{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.PiggyBankTest (tests) where

import Data.List
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude
import Fida.Contract.TestToolbox
  ( Run,
    Users (..),
    bad,
    good,
    insurancePolicy,
    newSamplePolicy,
    runUpdatePolicyState,
    setupUsers,
  )
import Fida.Contract.TestToolbox.TypedValidators (PiggyBank)
import Fida.Contract.TestToolbox.Action (buyFidaCards)
import Plutus.Model

tests :: TestTree
tests =
  testGroup
    "Unit tests for PiggyBank module"
    [ simpleTest
    , good "Buying Fida card works" testBuyFidaCard
    ]

simpleTest = testCase "sorting array" $ [1, 2, 3] @?= sort [2, 3, 1]


testBuyFidaCard :: Run ()
testBuyFidaCard = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  buyFidaCards iid users
