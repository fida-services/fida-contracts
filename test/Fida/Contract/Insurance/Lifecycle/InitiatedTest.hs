module Fida.Contract.Insurance.Lifecycle.InitiatedTest where

import Fida.Contract.Insurance.Lifecycle.Initiated
import Data.List
import Test.Tasty (TestTree, testGroup)
import Control.Monad (mapM, replicateM)
import Plutus.V2.Ledger.Api
import Test.Tasty.HUnit
import Plutus.Model
import PlutusTx.Prelude (Bool (..), Eq ((==)), return, ($), (&&), (.))

tests :: TestTree
tests =
  testGroup
    "Unit tests for Insurance.Lifecycle.Initiated module"
    [ good "User can exchange values" userExchange
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000_000) defaultBabbage


userExchange :: Run ()
userExchange = do
  users <- setupUsers
  let [u1, u2, u3] = users
  sendValue u1 (adaValue 100) u2
  sendValue u2 (adaValue 100) u3

-- | alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ adaValue 1000
