module Main (main) where

import qualified Fida.Contract.FidaPolicyContractTest as FidaPolicyContractTest
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Fida Contract Tests"
        [ FidaPolicyContractTest.tests
        ]
