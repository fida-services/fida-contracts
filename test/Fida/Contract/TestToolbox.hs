module Fida.Contract.TestToolbox
  ( module X,
    bad,
    good,
    Run,
    assertTrue,
  )
where

import Fida.Contract.TestToolbox.Action as X
import Fida.Contract.TestToolbox.Time as X
import Fida.Contract.TestToolbox.TypedValidators as X
import Fida.Contract.TestToolbox.Users as X
import Fida.Contract.TestToolbox.Investor as X
import Plutus.Model
  ( Run,
    adaValue,
    defaultBabbage,
    logError,
    mustFail,
    testNoErrors,
    testNoErrorsTrace,
  )
import Plutus.Model.Contract.Ext as X
import Test.Tasty (TestTree)
import Prelude

bad :: String -> Run a -> TestTree
bad msg = good msg . mustFail

good :: String -> Run a -> TestTree
good = testNoErrorsTrace (adaValue 1_000_000_000_000) defaultBabbage

assertTrue :: String -> Bool -> Run ()
assertTrue msg False = logError msg
assertTrue _ True = return ()
