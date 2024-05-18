module Fida.Contract.Utils (
    traceIfNotSingleton,
    mkUntypedMintingPolicy,
    count,
    lovelaceValueOf,
    output,
    outputDatum,
    unsafeOutputDatum,
    fromSingleton,
    unsafeFromSingleton,
    unsafeFromSingleton',
    maybeToList,
    unsafeUntypedOutputDatum,
    referenceDatums,
) where

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import qualified PlutusTx
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

-- TODO implement me
{-# INLINEABLE bsearch #-}
bsearch :: (Eq a, Ord a) => a -> [a] -> Bool
bsearch x xs = False

{-# INLINEABLE lovelaceValueOf #-}
lovelaceValueOf :: Value -> Integer
lovelaceValueOf v = valueOf v adaSymbol adaToken

{-# INLINEABLE fromSingleton #-}
fromSingleton :: [a] -> Maybe a
fromSingleton [a] = Just a
fromSingleton _ = Nothing

{-# INLINEABLE unsafeFromSingleton #-}
unsafeFromSingleton :: [a] -> a
unsafeFromSingleton [a] = a
unsafeFromSingleton [] = traceError "empty list"
unsafeFromSingleton _ = traceError "not singleton"

{-# INLINEABLE unsafeFromSingleton' #-}
unsafeFromSingleton' :: BuiltinString -> [a] -> a
unsafeFromSingleton' _ [a]  = a
unsafeFromSingleton' err [] = traceError $ err <> ".1"
unsafeFromSingleton' err _  = traceError $ err <> ".2"



{-# INLINEABLE output #-}
output :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> Maybe (TxOut, a)
output cs sc = fromSingleton . outputs cs sc

{-# INLINEABLE outputDatum #-}
outputDatum :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> Maybe a
outputDatum cs sc = fromSingleton . outputDatums cs sc

{-# INLINEABLE unsafeUntypedOutputDatum #-}
unsafeUntypedOutputDatum :: CurrencySymbol -> ScriptContext -> TokenName -> BuiltinData
unsafeUntypedOutputDatum cs sc = unsafeFromSingleton . untypedOutputDatums cs sc

{-# INLINEABLE untypedOutputDatums #-}
untypedOutputDatums :: CurrencySymbol -> ScriptContext -> TokenName -> [BuiltinData]
untypedOutputDatums cs sc tn =
    [ d
    | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
    , valueOf value cs tn == 1
    ]

{-# INLINEABLE outputDatums #-}
outputDatums :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [a]
outputDatums cs sc = map snd . outputs cs sc

{-# INLINEABLE outputs #-}
outputs :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [(TxOut,a)]
outputs cs sc = outputs' cs (getContinuingOutputs sc)

{-# INLINEABLE outputs' #-}
outputs' :: FromData a => CurrencySymbol -> [TxOut] -> TokenName -> [(TxOut,a)]
outputs' cs outs tn =
    [ (txOut, datum)
    | txOut@(TxOut _ value (OutputDatum (Datum d)) _) <- outs
    , valueOf value cs tn == 1
    , Just datum <- [PlutusTx.fromBuiltinData d]
    ]

{-# INLINEABLE referenceDatums #-}
referenceDatums :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [a]
referenceDatums cs sc = map snd . outputs' cs (getOuts sc)
  where
    getOuts =  map txInInfoResolved . txInfoReferenceInputs . scriptContextTxInfo

{-# INLINEABLE maybeToList #-}
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList _ = []

{-# INLINEABLE unsafeOutputDatum #-}
unsafeOutputDatum :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> a
unsafeOutputDatum cs sc = unsafeFromSingleton . outputDatums cs sc
