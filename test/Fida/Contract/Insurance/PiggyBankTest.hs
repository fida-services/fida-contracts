module Fida.Contract.Insurance.PiggyBankTest ( tests ) where

import Data.List
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unit tests for PiggyBank module"
    [ simpleTest
    ]

simpleTest = testCase "sorting array" $ [1, 2, 3] @?= sort [2, 3, 1]
