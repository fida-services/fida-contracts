module Main (main) where

import qualified Fida.Contract.FidaPolicyContractTest as FidaPolicyContractTest
import qualified Fida.Contract.Insurance.Lifecycle.InitiatedTest as InitiatedTest
import qualified Fida.Contract.Insurance.PiggyBankTest as PiggyBankTest
import qualified Fida.Contract.Insurance.Lifecycle.FundingTest as FundingTest
import qualified Fida.Contract.Insurance.Lifecycle.OnRiskTest as OnRiskTest
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Fida Contract Tests"
    [ FidaPolicyContractTest.tests
    , InitiatedTest.tests
    , PiggyBankTest.tests
    , FundingTest.tests
    , OnRiskTest.tests
    ]
