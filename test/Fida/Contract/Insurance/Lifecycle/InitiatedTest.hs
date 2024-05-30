{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.InitiatedTest (tests) where

import Control.Monad (mapM, replicateM)
import Data.List
import Fida.Contract.Insurance
import Fida.Contract.Insurance.Authority
import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId
import Fida.Contract.Insurance.Redeemer
import Fida.Contract.Insurance.PiggyBank
import Plutus.Model
import Plutus.V2.Ledger.Api
--import PlutusTx.Prelude (Bool (..), Eq ((==)), Integer, return, ($), (&&), (.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude
import Fida.Contract.Insurance.Tokens
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

tests :: TestTree
tests =
  testGroup
    "Unit tests for Insurance.Lifecycle.Initiated module"
    [ good "User can exchange values" userExchange
    ]
 where
  bad msg = good msg . mustFail
  good = testNoErrors (adaValue 10_000_000_000) defaultBabbage

type InsurancePolicy = TypedValidator InsurancePolicyDatum InsurancePolicyRedeemer

type PiggyBank = TypedValidator  PiggyBankDatum PiggyBankRedeemer

data InsuranceCreateParams = InsuranceCreateParams
  { icpPolicyHolder :: PubKeyHash
  , icpPolicyPrice :: Integer
  , icpFidaCardQuantity :: Integer
  , icpFidaCardValue :: Integer
  }

insurancePolicy :: InsuranceId -> InsurancePolicy
insurancePolicy = TypedValidator . toV2 . insurancePolicyValidator

piggyBank :: InsuranceId -> FidaCardId -> PiggyBank
piggyBank iid = TypedValidator . toV2 . piggyBankValidator iid 

piggyBankAddr :: InsuranceId -> FidaCardId -> Address
piggyBankAddr iid = toAddress . piggyBank iid

insuranceIdNFT :: TxOutRef -> TypedPolicy ()
insuranceIdNFT = TypedPolicy . toV2 . insuranceIdMintingPolicy

userExchange :: Run ()
userExchange = do
  users <- setupUsers
  let [u1, u2, u3, u4, u5] = users
  sendValue u1 (adaValue 100) u2
  sendValue u2 (adaValue 100) u3

setupUsers :: Run [PubKeyHash]
setupUsers = do
  users@[u1, u2, u3, u4, u5] <- replicateM 5 $ newUser $ adaValue 100_000_000
  writeUserName u1 "Policy Broker"
  writeUserName u2 "Policy Holder"
  writeUserName u3 "Investor 1"
  writeUserName u4 "Investor 2"
  writeUserName u5 "Investor 3"
  return users

fidaCardFromInt :: Integer -> FidaCardId
fidaCardFromInt = FidaCardId . stringToBuiltinByteString . show

iinfoFromParams :: InsuranceCreateParams -> InsurancePolicyDatum
iinfoFromParams InsuranceCreateParams {..} =
  let iInfoClaim = Nothing
  in InsuranceInfo {..}


makePolicy :: PubKeyHash -> InsuranceCreateParams -> Run InsuranceId
makePolicy broker params@InsuranceCreateParams {..} = do
  utxos <- utxoAt broker
  let
    [(ref, out)] = utxos
    insuranceIdNFTScript = insuranceIdNFT ref
    iid = InsuranceId $ scriptCurrencySymbol insuranceIdNFTScript
    insuranceScript = insurancePolicy iid
    tx = mconcat $
          [ payToScript insuranceScript (InlineDatum iinfo) (policyInfoNFT iid)
          , payToScript insuranceScript (InlineDatum $ paymentInfo iid) (policyPaymentNFT iid)
          , spendPubKey ref
          , mintValue insuranceIdNFTScript () (policyInfoNFT iid <> policyPaymentNFT iid)
          ] <> payToPgiggyBanks insuranceIdNFTScript iid
  submitTx broker tx
  return iid
  where
    iinfo = iinfoFromParams params

    payToPgiggyBanks script iid =
      map (payToPiggyBank script iid . fidaCardFromInt) [1..icpFidaCardQuantity]
    
    fidaCardDatum fcid =
      let pbfcIsSold = False
          pbfcFidaCardId = fcid
          pbfcFidaCardValue = icpFidaCardValue
          pbfcPaidClaims = []
      in  PBankFidaCard {..}

    payToPiggyBank insuranceIdNFTScript iid fcid =
      let script = piggyBank iid fcid
          value = fidaCardStatusNFT iid <> fidaCardNFT iid fcid
      in  mconcat
            [ payToScript script (InlineDatum $ fidaCardDatum fcid) value
            , mintValue insuranceIdNFTScript () value
            ]
    
    paymentInfo iid =
      let
         ppInfoPremiumAmountPerPiggyBank = icpPolicyPrice `div` icpFidaCardQuantity
         ppInfoPiggyBanks =  map (piggyBankAddr iid . fidaCardFromInt) [1..icpFidaCardQuantity]
      in PremiumPaymentInfo {..}


policyInfoNFT :: InsuranceId -> Value
policyInfoNFT (InsuranceId cs) = singleton cs policyInfoTokenName 1

policyPaymentNFT :: InsuranceId -> Value
policyPaymentNFT (InsuranceId cs) = singleton cs policyPaymentTokenName 1

fidaCardNFT :: InsuranceId -> FidaCardId -> Value
fidaCardNFT (InsuranceId cs) (FidaCardId fcid) = singleton cs (fidaCardTokenName fcid) 1

fidaCardStatusNFT :: InsuranceId -> Value
fidaCardStatusNFT (InsuranceId cs) = singleton cs fidaCardStatusTokenName 1
