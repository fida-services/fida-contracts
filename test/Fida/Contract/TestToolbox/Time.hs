module Fida.Contract.TestToolbox.Time
  ( beginningOfTime
  , days
  ) where

import Plutus.V1.Ledger.Time (DiffMilliSeconds (..))
import Plutus.V2.Ledger.Api (POSIXTime (..))
import Prelude

-- | Friday, 1 March 2024 12:12:12
beginningOfTime :: POSIXTime
beginningOfTime = POSIXTime 1709295132000

days :: Integer -> DiffMilliSeconds
days n = DiffMilliSeconds $ 1000 * 60 * 60 * 24 * n
