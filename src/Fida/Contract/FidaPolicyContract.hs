{-# LANGUAGE TemplateHaskell #-}

module Fida.Contract.FidaPolicyContract where

import Fida.Contract.Types
import Ledger (Language (PlutusV2), Versioned (Versioned))
import Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import Plutus.V2.Ledger.Api
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified

{-# INLINEABLE policyDataTokenName #-}
policyDataTokenName :: TokenName
policyDataTokenName = TokenName "POLICY"

{-# INLINEABLE fidaCardTokenName #-}
fidaCardTokenName :: TokenName
fidaCardTokenName = TokenName "CARD"

{-# INLINEABLE policyHolderTokenName #-}
policyHolderTokenName :: TokenName
policyHolderTokenName = TokenName "POLICY-HOLDER"

{-# INLINEABLE policyHolderDatumTokenName #-}
policyHolderDatumTokenName :: TokenName
policyHolderDatumTokenName = TokenName "POLICY-HOLDER-DATUM"

{-# INLINEABLE policyInvestorDatumTokenName #-}
policyInvestorDatumTokenName :: TokenName
policyInvestorDatumTokenName = TokenName "INVESTOR-DATUM"

{-# INLINEABLE mkFidaContractMintingPolicy #-}
mkFidaContractMintingPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkFidaContractMintingPolicy ref _ (ScriptContext txInfo (Minting _)) =
  traceIfFalse "ERROR-CONTRACT-MINTING-POLICY-0" isTxOutRefUsed
  where
    -- Ensure that the TxOutRef is used by the transaction.
    -- This parameter ensures the uniqueness of the currency symbol.
    isTxOutRefUsed =
      ref `elem` map txInInfoOutRef (txInfoInputs txInfo)
mkFidaContractMintingPolicy _ _ _ =
  trace "ERROR-CONTRACT-MINTING-POLICY-1" False

{-# INLINEABLE mkFidaContractMintingPolicyUntyped #-}
mkFidaContractMintingPolicyUntyped ::
  -- | TxOutRef
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkFidaContractMintingPolicyUntyped ref =
  mkUntypedMintingPolicy $
    mkFidaContractMintingPolicy
      (unsafeFromBuiltinData ref)

serialisableFidaContractMintingPolicy :: Versioned Script
serialisableFidaContractMintingPolicy =
  Versioned
    (fromCompiledCode $$(PlutusTx.compile [||mkFidaContractMintingPolicyUntyped||]))
    PlutusV2

data FidaContractRedeemer = BuyFidaCard Integer | PayPremium Integer | Activate

PlutusTx.makeIsDataIndexed
  ''FidaContractRedeemer
  [ ('BuyFidaCard, 0)
  , ('PayPremium, 1)
  , ('Activate, 2)
  ]

data FidaContractState = Initialized | OnRisk deriving (Prelude.Show)

PlutusTx.makeIsDataIndexed
  ''FidaContractState
  [ ('Initialized, 0)
  , ('OnRisk, 1)
  ]

data FidaContractDatum = FidaContractDatum
  { collateralAmount :: Integer
  , fidaCardValue :: Integer
  , premiumAmount :: Integer
  , policyHolder :: Address
  , policyAuthority :: PolicyAuthority
  , startDate :: (Maybe POSIXTime)
  , paymentIntervals :: Integer
  , contractState :: FidaContractState
  }
  deriving (Prelude.Show)

PlutusTx.makeIsDataIndexed ''FidaContractDatum [('FidaContractDatum, 0)]

{-# INLINEABLE mkFidaContractValidator #-}
mkFidaContractValidator ::
  PolicyId ->
  FidaContractDatum ->
  FidaContractRedeemer ->
  ScriptContext ->
  Bool
mkFidaContractValidator _ _ _ _ = True

{-# INLINEABLE mkFidaContractValidatorUntyped #-}
mkFidaContractValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkFidaContractValidatorUntyped policyId datum redeemer scriptContext =
  check $
    mkFidaContractValidator
      (unsafeFromBuiltinData policyId)
      (unsafeFromBuiltinData datum)
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData scriptContext)

serialisableFidaContractValidator :: Versioned Script
serialisableFidaContractValidator =
  Versioned
    (fromCompiledCode $$(PlutusTx.compile [||mkFidaContractValidatorUntyped||]))
    PlutusV2
