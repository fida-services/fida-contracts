module Fida.Contract.Utils where

import Data.Kind (Type)
import Plutus.V2.Ledger.Api
import PlutusTx (
    FromData (fromBuiltinData),
    ToData (toBuiltinData),
    UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Prelude

{- | Verify that a list contains only a single element,
   or generate an error if it does not.
-}
{-# INLINEABLE assertSingleton #-}
assertSingleton :: BuiltinString -> [a] -> a
assertSingleton _ [x] = x
assertSingleton msg _ = traceError msg

{-# INLINE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy ::
    forall (r :: Type).
    (UnsafeFromData r) =>
    (r -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if
-- parsing failed
mkUntypedMintingPolicy f r p =
    check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
