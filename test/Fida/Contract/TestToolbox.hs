module Fida.Contract.TestToolbox
  ( module X,
    bad,
    good,
    Run
  )
where

import Fida.Contract.TestToolbox.Action as X
import Fida.Contract.TestToolbox.Time as X
import Fida.Contract.TestToolbox.TypedValidators as X
import Fida.Contract.TestToolbox.Users as X
import Plutus.Model.Contract.Ext as X
import Plutus.Model
  ( Run,
    adaValue,
    defaultBabbage,
    mustFail,
    testNoErrors,
    testNoErrorsTrace,
  )
import Test.Tasty (TestTree)
import Prelude

bad :: String -> Run a -> TestTree
bad msg = good msg . mustFail

good :: String -> Run a -> TestTree
good = testNoErrors (adaValue 1000_000_000_000) defaultBabbage
