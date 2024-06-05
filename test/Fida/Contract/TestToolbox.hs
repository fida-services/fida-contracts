module Fida.Contract.TestToolbox
  ( module X,
    bad,
    good,
    Run,
    payToAddressDatum,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Fida.Contract.TestToolbox.Action as X
import Fida.Contract.TestToolbox.Time as X
import Fida.Contract.TestToolbox.TypedValidators as X
import Fida.Contract.TestToolbox.Users as X
import Plutus.Model
  ( DatumMode (..),
    DatumType (..),
    HasDatum,
    Run,
    adaValue,
    datumHash,
    defaultBabbage,
    mustFail,
    testNoErrors,
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
    Value,
    toBuiltinData,
  )
import Test.Tasty (TestTree)
import Prelude

bad :: String -> Run a -> TestTree
bad msg = good msg . mustFail

good :: String -> Run a -> TestTree
good = testNoErrors (adaValue 1000_000_000_000) defaultBabbage

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
--
fromDatumMode :: ToData a => DatumMode a -> (OutputDatum, Map DatumHash Datum)
fromDatumMode = \case
  HashDatum dat ->
    let dh = datumHash datum
        datum = Datum $ toBuiltinData dat
     in (OutputDatumHash dh, M.singleton dh datum)
  InlineDatum dat ->
    let datum = Datum $ toBuiltinData dat
     in (OutputDatum datum, M.empty)
