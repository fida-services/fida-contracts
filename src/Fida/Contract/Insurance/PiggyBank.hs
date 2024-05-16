{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Fida.Contract.Insurance.PiggyBank (
    serialisablePiggyBankValidator,
) where

import Fida.Contract.Insurance.Datum (PiggyBankDatum(..))
import Fida.Contract.Insurance.Identifier (InsuranceId(..))
import Fida.Contract.Insurance.Redeemer (PiggyBankRedeemer(..))
import Fida.Contract.Insurance.Tokens (fidaCardTokenName, fidaCardStatusTokenName)
import Fida.Contract.Utils (outputDatum, fromSingleton, lovelaceValueOf, output)
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Contexts (getContinuingOutputs, findOwnInput)

newtype FidaCardId = FidaCardId Integer
    deriving newtype (ToData, FromData, UnsafeFromData)

{- |

 On chain errors:

  ERROR-PIGGY-BANK-VALIDATOR-0: output datum doesn't say that the card is sold

  ERROR-PIGGY-BANK-VALIDATOR-1: output datum doesn't match input datum

  ERROR-PIGGY-BANK-VALIDATOR-2: own input not found

  ERROR-PIGGY-BANK-VALIDATOR-3: correct output datum not found

  ERROR-PIGGY-BANK-VALIDATOR-4: fida card not paid for

  ERROR-PIGGY-BANK-VALIDATOR-5: output datum not found

  ERROR-PIGGY-BANK-VALIDATOR-6: output datum says that the card is sold

  ERROR-PIGGY-BANK-VALIDATOR-7: output datum doesn't match input datum

  ERROR-PIGGY-BANK-VALIDATOR-8: datum not found
-}
{-# INLINEABLE mkPiggyBankValidator #-}
mkPiggyBankValidator ::
    InsuranceId ->
    FidaCardId ->
    PiggyBankDatum ->
    PiggyBankRedeemer ->
    ScriptContext ->
    Bool
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=False, pbfcFidaCardValue}) BuyFidaCard scriptContext =
        traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-0" pbfcIsSold
        && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-1" (pbfcFidaCardValue == pbfcFidaCardValue')
    where
        inputLovelace = case findOwnInput scriptContext of
            Just (TxInInfo _ (TxOut _ value _ _)) -> lovelaceValueOf value
            Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-2"

        (PBankFidaCard {pbfcIsSold, pbfcFidaCardValue=pbfcFidaCardValue'}) =
                case output cs scriptContext (fidaCardStatusTokenName) of
                    Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-3"
                    Just (TxOut _ v _ _, d) | lovelaceValueOf v < pbfcFidaCardValue + inputLovelace ->
                        traceError "ERROR-PIGGY-BANK-VALIDATOR-4"
                                            | otherwise -> d
                    _ -> traceError "ERROR-PIGGY-BANK-VALIDATOR-5"
mkPiggyBankValidator (InsuranceId cs) (FidaCardId n) (PBankFidaCard {pbfcIsSold=True, pbfcFidaCardValue}) SellFidaCard scriptContext =
        traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-6" (not pbfcIsSold)
        && traceIfFalse "ERROR-PIGGY-BANK-VALIDATOR-7" (pbfcFidaCardValue == pbfcFidaCardValue')
    where datum = fromSingleton
                    [ datum
                    | TxOut _ value (OutputDatum (Datum d)) _ <- getContinuingOutputs scriptContext
                    , valueOf value cs (fidaCardTokenName n) == 1
                    , valueOf value cs fidaCardStatusTokenName == 1
                    , Just datum <- [PlutusTx.fromBuiltinData d]
                    ]
          (PBankFidaCard {pbfcIsSold, pbfcFidaCardValue=pbfcFidaCardValue'}) =
                case datum of
                    Just x -> x
                    Nothing -> traceError "ERROR-PIGGY-BANK-VALIDATOR-8"
mkPiggyBankValidator _ _ _ ClaimPremium _ = False
mkPiggyBankValidator _ _ _ UnlockCollateral _ = False
mkPiggyBankValidator _ _ _ ClaimPremiumOnCancel _ = False
mkPiggyBankValidator _ _ _ UnlockCollateralOnCancel _ = False
mkPiggyBankValidator _ _ _ _ _ = False


{-# INLINEABLE mkPiggyBankValidatorUntyped #-}
mkPiggyBankValidatorUntyped ::
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
mkPiggyBankValidatorUntyped insuranceId fidaCardId datum redeemer sc =
    check $
        mkPiggyBankValidator
            (unsafeFromBuiltinData insuranceId)
            (unsafeFromBuiltinData fidaCardId)
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData sc)

serialisablePiggyBankValidator :: Script
serialisablePiggyBankValidator =
    fromCompiledCode $$(PlutusTx.compile [||mkPiggyBankValidatorUntyped||])
