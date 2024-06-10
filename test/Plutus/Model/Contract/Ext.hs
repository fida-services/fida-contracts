{-# LANGUAGE RecordWildCards #-}

module Plutus.Model.Contract.Ext
  ( spendBoxRef,
    payToAddressDatum,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Plutus.Model
  ( DatumMode (..),
    DatumType (..),
    HasDatum,
    IsValidator,
    RedeemerType,
    Run,
    TxBox (..),
    adaValue,
    datumHash,
    defaultBabbage,
    mustFail,
    spendScriptRef,
    testNoErrorsTrace,
  )
import qualified Plutus.Model.Fork.Ledger.Tx as P
import Plutus.Model.Fork.TxExtra (Tx, toExtra)
import Plutus.V2.Ledger.Api
  ( Address,
    Datum (..),
    DatumHash (..),
    OutputDatum (..),
    PubKeyHash,
    ToData,
    TxOut (..),
    TxOutRef (..),
    Value,
    toBuiltinData,
  )
import Test.Tasty (TestTree)
import Prelude

spendBoxRef ::
  (IsValidator script) =>
  TxOutRef ->
  script ->
  RedeemerType script ->
  TxBox script ->
  Tx
spendBoxRef refScript tv red TxBox {..} =
  spendScriptRef refScript tv txBoxRef red txBoxDatum

payToAddressDatum ::
  (ToData a) =>
  Address ->
  DatumMode a ->
  Value ->
  Tx
payToAddressDatum address dat val =
  toExtra $
    mempty
      { P.txOutputs = [TxOut address val outDatum Nothing]
      , P.txData = datumMap
      }
 where
  (outDatum, datumMap) = fromDatumMode dat

-- | Copied from Plutus.Model.Contract
fromDatumMode :: ToData a => DatumMode a -> (OutputDatum, Map DatumHash Datum)
fromDatumMode = \case
  HashDatum dat ->
    let dh = datumHash datum
        datum = Datum $ toBuiltinData dat
     in (OutputDatumHash dh, M.singleton dh datum)
  InlineDatum dat ->
    let datum = Datum $ toBuiltinData dat
     in (OutputDatum datum, M.empty)
