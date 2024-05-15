{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Authority (
    InsuranceAuthority,
    isSignedByTheAuthority,
) where

import Fida.Contract.Utils (count)
import Plutus.V2.Ledger.Api (PubKeyHash)
import Plutus.V2.Ledger.Contexts (ScriptContext (..), txSignedBy)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as HPrelude

data InsuranceAuthority
    = SingleSign PubKeyHash
    | AtLeastOneSign [PubKeyHash]
    | AllMustSign [PubKeyHash]
    | MajorityMustSign [PubKeyHash]
    deriving (HPrelude.Show)

PlutusTx.makeIsDataIndexed
    ''InsuranceAuthority
    [ ('SingleSign, 0)
    , ('AtLeastOneSign, 1)
    , ('AllMustSign, 2)
    , ('MajorityMustSign, 3)
    ]

{-# INLINEABLE isSignedByTheAuthority #-}
isSignedByTheAuthority :: ScriptContext -> InsuranceAuthority -> Bool
isSignedByTheAuthority sc auth =
    case auth of
        SingleSign pkh -> txSignedBy txInfo pkh
        AtLeastOneSign pkhs -> signedBy pkhs == 1
        AllMustSign pkhs -> signedBy pkhs == length pkhs
        MajorityMustSign pkhs -> 2 * signedBy pkhs > length pkhs
  where
    txInfo = scriptContextTxInfo sc
    signedBy :: [PubKeyHash] -> Integer
    signedBy = count id . map (txSignedBy txInfo)
