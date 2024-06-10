module Fida.Contract.TestToolbox.TypedValidators
  ( InsurancePolicy,
    PiggyBank,
    insurancePolicy,
    piggyBank,
    insuranceIdNFT,
    piggyBankAddr,
    policyInfoNFT,
    policyPaymentNFT,
    fidaCardNFT,
    fidaCardStatusNFT,
    fidaCardFromInt,
    iinfoBox,
    ppInfoBox,
    runLoadRefScript,
    isScriptRef,
    piggyBankInfoBox
  )
where

import Fida.Contract.Insurance (insurancePolicyValidator)
import Fida.Contract.Insurance.Datum (FidaCardId (..), InsurancePolicyDatum, PiggyBankDatum)
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..), insuranceIdMintingPolicy)
import Fida.Contract.Insurance.PiggyBank (piggyBankValidator)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer, PiggyBankRedeemer)
import Fida.Contract.Insurance.Tokens
  ( fidaCardStatusTokenName,
    fidaCardTokenName,
    policyInfoTokenName,
    policyPaymentTokenName,
  )
import Plutus.Model
  ( IsValidator,
    Run,
    TxBox (..),
    TypedPolicy (..),
    TypedValidator (..),
    HasValidatorHash (..),
    adaValue,
    loadRefScript,
    spend,
    submitTx,
    toAddress,
    toV2,
    userSpend,
  )
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (Address, PubKeyHash, TxOut (..), TxOutRef (..), Value, singleton,
                             ScriptHash (..), ValidatorHash (..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Prelude

-- | Typed validators
type InsurancePolicy = TypedValidator InsurancePolicyDatum InsurancePolicyRedeemer

type PiggyBank = TypedValidator PiggyBankDatum PiggyBankRedeemer

insurancePolicy :: InsuranceId -> InsurancePolicy
insurancePolicy = TypedValidator . toV2 . insurancePolicyValidator

piggyBank :: InsuranceId -> FidaCardId -> PiggyBank
piggyBank iid = TypedValidator . toV2 . piggyBankValidator iid

insuranceIdNFT :: TxOutRef -> TypedPolicy ()
insuranceIdNFT = TypedPolicy . toV2 . insuranceIdMintingPolicy

-- | Validator addresses
piggyBankAddr :: InsuranceId -> FidaCardId -> Address
piggyBankAddr iid = toAddress . piggyBank iid

-- | Validator NFTs
policyInfoNFT :: InsuranceId -> Value
policyInfoNFT (InsuranceId cs) = singleton cs policyInfoTokenName 1

policyPaymentNFT :: InsuranceId -> Value
policyPaymentNFT (InsuranceId cs) = singleton cs policyPaymentTokenName 1

fidaCardNFT :: InsuranceId -> FidaCardId -> Value
fidaCardNFT (InsuranceId cs) (FidaCardId fcid) = singleton cs (fidaCardTokenName fcid) 1

fidaCardStatusNFT :: InsuranceId -> Value
fidaCardStatusNFT (InsuranceId cs) = singleton cs fidaCardStatusTokenName 1

fidaCardFromInt :: Integer -> FidaCardId
fidaCardFromInt = FidaCardId . stringToBuiltinByteString . show

-- | Helper functions related to tv
isScriptRef :: HasValidatorHash script => script -> (TxOutRef, TxOut) -> Bool
isScriptRef script (_, TxOut _ _ _ (Just (ScriptHash hash))) =
  let ValidatorHash hash' = toValidatorHash script
   in hash' == hash
isScriptRef _ _ = False

iinfoBox :: InsuranceId -> TxBox script -> Bool
iinfoBox (InsuranceId cs) (TxBox _ (TxOut _ value _ _) _) =
  valueOf value cs policyInfoTokenName == 1

ppInfoBox :: InsuranceId -> TxBox script -> Bool
ppInfoBox (InsuranceId cs) (TxBox _ (TxOut _ value _ _) _) =
  valueOf value cs policyPaymentTokenName == 1

piggyBankInfoBox :: InsuranceId -> FidaCardId -> TxBox script -> Bool
piggyBankInfoBox (InsuranceId cs) (FidaCardId fcid) (TxBox _ (TxOut _ value _ _) _) =
  valueOf value cs (fidaCardTokenName fcid) == 1

runLoadRefScript ::
  (IsValidator script) =>
  PubKeyHash ->
  script ->
  Run ()
runLoadRefScript pkh script = do
  sp <- spend pkh $ adaValue 1
  submitTx pkh $
    mconcat
      [ userSpend sp
      , loadRefScript script (adaValue 1)
      ]
