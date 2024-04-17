module Fida.Contract.Utils where

import PlutusTx.Prelude

{- | Verify that a list contains only a single element,
   or generate an error if it does not.
-}
{-# INLINEABLE assertSingleton #-}
assertSingleton :: BuiltinString -> [a] -> a
assertSingleton _ [x] = x
assertSingleton msg _ = traceError msg
