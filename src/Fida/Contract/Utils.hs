module Fida.Contract.Utils (
    traceIfNotSingleton,
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
    untypedOutputDatum,
    referenceDatums,
    unsafeReferenceDatum,
    findOutputDatumByType,
    findOutputDatumsByType,
    unsafeReferenceOutput,
    referenceOutputs,
    wrapPolicy,
    wrapStakeValidator,
    wrapValidator,
    unsafeFromJust,
) where

import Plutus.V1.Ledger.Value (
    CurrencySymbol,
    TokenName,
    Value,
    adaSymbol,
    adaToken,
    valueOf,
 )
import Plutus.V2.Ledger.Api (
    Datum (..),
    FromData,
    OutputDatum (..),
    ScriptContext (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    UnsafeFromData (..),
 )
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import qualified PlutusTx
import PlutusTx.Prelude

-- | common
{-# INLINEABLE unsafeFromJust #-}
unsafeFromJust :: Maybe a -> a
unsafeFromJust (Just a) = a
unsafeFromJust Nothing = traceError "nothing"

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
unsafeFromSingleton' _ [a] = a
unsafeFromSingleton' err [] = traceError $ err <> ".1"
unsafeFromSingleton' err _ = traceError $ err <> ".2"

{-# INLINEABLE traceIfNotSingleton #-}
traceIfNotSingleton :: BuiltinString -> [a] -> a
traceIfNotSingleton _ [x] = x
traceIfNotSingleton msg _ = traceError msg

{-# INLINEABLE maybeToList #-}
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList _ = []

{-# INLINEABLE count #-}
count :: (a -> Bool) -> [a] -> Integer
count p = go 0
  where
    go n [] = n
    go n (x : xs)
        | p x = go (n + 1) xs
        | otherwise = go n xs

-- | wrappers
{-# INLINEABLE wrapValidator #-}
wrapValidator ::
    (UnsafeFromData d, UnsafeFromData r) =>
    (d -> r -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f d r ctx =
    check $
        f
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapPolicy #-}
wrapPolicy ::
    UnsafeFromData r =>
    (r -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> ())
wrapPolicy f r ctx =
    check $
        f
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapStakeValidator #-}
wrapStakeValidator ::
    UnsafeFromData r =>
    (r -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> ())
wrapStakeValidator = wrapPolicy

-- | outputs
{-# INLINEABLE untypedOutputDatums #-}
untypedOutputDatums :: CurrencySymbol -> ScriptContext -> TokenName -> [BuiltinData]
untypedOutputDatums cs sc tn =
    [ d
    | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
    , valueOf value cs tn == 1
    ]

{-# INLINEABLE untypedOutputDatum #-}
untypedOutputDatum :: CurrencySymbol -> ScriptContext -> TokenName -> Maybe BuiltinData
untypedOutputDatum cs sc = fromSingleton . untypedOutputDatums cs sc

{-# INLINEABLE unsafeUntypedOutputDatum #-}
unsafeUntypedOutputDatum :: CurrencySymbol -> ScriptContext -> TokenName -> BuiltinData
unsafeUntypedOutputDatum cs sc = unsafeFromSingleton . untypedOutputDatums cs sc

{-# INLINEABLE outputs' #-}
outputs' :: FromData a => CurrencySymbol -> [TxOut] -> TokenName -> [(TxOut, a)]
outputs' cs outs tn =
    [ (txOut, datum)
    | txOut@(TxOut _ value (OutputDatum (Datum d)) _) <- outs
    , valueOf value cs tn == 1
    , Just datum <- [PlutusTx.fromBuiltinData d]
    ]

{-# INLINEABLE outputs #-}
outputs :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [(TxOut, a)]
outputs cs sc = outputs' cs (getContinuingOutputs sc)

{-# INLINEABLE output #-}
output :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> Maybe (TxOut, a)
output cs sc = fromSingleton . outputs cs sc

{-# INLINEABLE outputDatums #-}
outputDatums :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [a]
outputDatums cs sc = map snd . outputs cs sc

{-# INLINEABLE outputDatum #-}
outputDatum :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> Maybe a
outputDatum cs sc = fromSingleton . outputDatums cs sc

{-# INLINEABLE unsafeOutputDatum #-}
unsafeOutputDatum :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> a
unsafeOutputDatum cs sc = unsafeFromSingleton . outputDatums cs sc

{-# INLINEABLE referenceOutputs #-}
referenceOutputs :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [(TxOut, a)]
referenceOutputs cs sc = outputs' cs (getOuts sc)
  where
    getOuts = map txInInfoResolved . txInfoReferenceInputs . scriptContextTxInfo

{-# INLINEABLE unsafeReferenceOutput #-}
unsafeReferenceOutput :: FromData a => BuiltinString -> CurrencySymbol -> ScriptContext -> TokenName -> (TxOut, a)
unsafeReferenceOutput err cs sc = unsafeFromSingleton' err . referenceOutputs cs sc

{-# INLINEABLE referenceDatums #-}
referenceDatums :: FromData a => CurrencySymbol -> ScriptContext -> TokenName -> [a]
referenceDatums cs sc = map snd . referenceOutputs cs sc

{-# INLINEABLE unsafeReferenceDatum #-}
unsafeReferenceDatum :: FromData a => BuiltinString -> CurrencySymbol -> ScriptContext -> TokenName -> a
unsafeReferenceDatum err cs sc = unsafeFromSingleton' err . referenceDatums cs sc

{-# INLINEABLE findOutputDatumsByType #-}
findOutputDatumsByType :: FromData a => ScriptContext -> [a]
findOutputDatumsByType sc =
    [ datum
    | TxOut _ _value (OutputDatum (Datum d)) _ <- getContinuingOutputs sc
    , Just datum <- [PlutusTx.fromBuiltinData d]
    ]

{-# INLINEABLE findOutputDatumByType #-}
findOutputDatumByType :: FromData a => ScriptContext -> Maybe a
findOutputDatumByType sc = fromSingleton $ findOutputDatumsByType sc
