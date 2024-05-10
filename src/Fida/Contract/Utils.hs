module Fida.Contract.Utils (
    traceIfNotSingleton,
    mkUntypedMintingPolicy,
    count,
) where

import Plutus.V2.Ledger.Api (ScriptContext, UnsafeFromData (..))
import PlutusTx.Prelude

{- | Verify that a list contains only a single element,
   or generate an error if it does not.
-}
{-# INLINEABLE traceIfNotSingleton #-}
traceIfNotSingleton :: BuiltinString -> [a] -> a
traceIfNotSingleton _ [x] = x
traceIfNotSingleton msg _ = traceError msg

{-# INLINE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy ::
    forall r.
    (UnsafeFromData r) =>
    (r -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> ())
mkUntypedMintingPolicy f r s =
    check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData s)

{-# INLINEABLE count #-}
count :: (a -> Bool) -> [a] -> Integer
count p = go 0
  where
    go n [] = n
    go n (x : xs)
        | p x = go (n + 1) xs
        | otherwise = go n xs
