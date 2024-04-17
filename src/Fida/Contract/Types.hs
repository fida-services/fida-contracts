{-# LANGUAGE TemplateHaskell #-}

-- | The types that are shared across other modules.
module Fida.Contract.Types where

import Plutus.V2.Ledger.Api (BuiltinByteString, CurrencySymbol, PubKeyHash)
import PlutusTx (FromData, ToData, UnsafeFromData)
import PlutusTx qualified
import Prelude qualified

-- | System governance authority strategy
data PolicyAuthority
  = SingleSign !PubKeyHash
  | AtLeastOneSign ![PubKeyHash]
  | AllMustSign ![PubKeyHash]
  | MajorityMustSign ![PubKeyHash]
  deriving (Prelude.Show)

PlutusTx.makeLift ''PolicyAuthority
PlutusTx.makeIsDataIndexed
  ''PolicyAuthority
  [ ('SingleSign, 0)
  , ('AtLeastOneSign, 1)
  , ('AllMustSign, 2)
  , ('MajorityMustSign, 3)
  ]

-- | Unique Fida Policy identifier
newtype PolicyId = PolicyId CurrencySymbol
  deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)

PlutusTx.makeLift ''PolicyId

newtype SystemId = SystemId CurrencySymbol
  deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)

PlutusTx.makeLift ''SystemId

newtype SystemGovernance = SystemGovernance PubKeyHash
  deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData)
