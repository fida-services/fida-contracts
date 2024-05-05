{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.Insurance.Authority (
    InsuranceAuthority,
) where

import Plutus.V2.Ledger.Api (PubKeyHash)
import qualified PlutusTx
import qualified Prelude

data InsuranceAuthority
    = SingleSign !PubKeyHash
    | AtLeastOneSign ![PubKeyHash]
    | AllMustSign ![PubKeyHash]
    | MajorityMustSign ![PubKeyHash]
    deriving (Prelude.Show)

PlutusTx.makeIsDataIndexed
    ''InsuranceAuthority
    [ ('SingleSign, 0)
    , ('AtLeastOneSign, 1)
    , ('AllMustSign, 2)
    , ('MajorityMustSign, 3)
    ]
