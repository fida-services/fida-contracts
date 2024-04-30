module Fida.Contract.Utils where

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
    forall r.
    (UnsafeFromData r) =>
    (r -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> ())
mkUntypedMintingPolicy f r s =
    check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData s)
