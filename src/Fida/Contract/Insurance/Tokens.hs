module Fida.Contract.Insurance.Tokens (
    insuranceDatumTokenName,
    fidaCardTokenName,
) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Plutus.V2.Ledger.Api (TokenName (..))
import PlutusTx.Prelude
import Text.Show (Show (show))

{-# INLINEABLE insuranceDatumTokenName #-}
insuranceDatumTokenName :: TokenName
insuranceDatumTokenName = TokenName "POLICY"

{-# INLINEABLE fidaCardTokenName #-}
fidaCardTokenName :: Integer -> TokenName
fidaCardTokenName identifier = TokenName $ "CARD-" <> int2BBString identifier

{-# INLINEABLE int2BBString #-}
int2BBString :: Integer -> BuiltinByteString
int2BBString = toBuiltin . Text.encodeUtf8 . Text.pack . show
