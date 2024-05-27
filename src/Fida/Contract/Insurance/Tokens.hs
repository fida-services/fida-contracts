module Fida.Contract.Insurance.Tokens
  ( policyInfoTokenName,
    fidaCardTokenName,
    fidaCardStatusTokenName,
    policyPaymentTokenName,
  )
where

import Plutus.V2.Ledger.Api (TokenName (..))
import PlutusTx.Prelude

{-# INLINEABLE policyInfoTokenName #-}
policyInfoTokenName :: TokenName
policyInfoTokenName = TokenName "POLICY-INFO"

{-# INLINEABLE policyPaymentTokenName #-}
policyPaymentTokenName :: TokenName
policyPaymentTokenName = TokenName "POLICY-PAYMENT"

{-# INLINEABLE fidaCardTokenName #-}
fidaCardTokenName :: BuiltinByteString -> TokenName
fidaCardTokenName identifier = TokenName $ "CARD-" <> identifier

{-# INLINEABLE fidaCardStatusTokenName #-}
fidaCardStatusTokenName :: TokenName
fidaCardStatusTokenName = TokenName "CARD-STATUS"
