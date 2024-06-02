{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fida.Contract.Insurance.Lifecycle.InitiatedTest (tests) where

import Control.Monad (mapM, replicateM, void)
import Fida.Contract.Insurance
import Fida.Contract.Insurance.Authority
import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId
import Fida.Contract.Insurance.PiggyBank
import Fida.Contract.Insurance.Redeemer
import Fida.Contract.Insurance.Tokens
import Plutus.Model hiding (days)
import Plutus.V1.Ledger.Time (DiffMilliSeconds (..), fromMilliSeconds)
import Plutus.V1.Ledger.Value (scale, valueOf)
import Plutus.V2.Ledger.Api
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unit tests for Insurance.Lifecycle.Initiated module"
    [ good "Creating policy works" testCreatePolicy
    , good "Broker is allowed to cancell a policy" testCancelPolicyByBroker
    , bad "Unauthorized user is not allowed to cancell a policy" testCancelPolicyByUnauthorizedUser
    , bad "Cancelling policy can't set state to Funding" $ testCancelPolicyIfIllegalState Funding
    , bad "Cancelling policy can't set state to OnRisk" $ testCancelPolicyIfIllegalState OnRisk
    , bad "Cancelling policy can't set state to Initiated" $ testCancelPolicyIfIllegalState Initiated
    ]
 where
  bad msg = good msg . mustFail
  good = testNoErrors (adaValue 1000_000_000_000) defaultBabbage

testCreatePolicy :: Run ()
testCreatePolicy = void $ setupUsers >>= newSamplePolicy

runUpdatePolicyState ::
  InsurancePolicyState ->
  InsurancePolicyRedeemer ->
  InsuranceId ->
  PubKeyHash ->
  Run ()
runUpdatePolicyState state r iid pkh = do
  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $
    \box@(TxBox _ (TxOut _ value _ _) iinfo) -> do
      let maybeIinfo = updatePolicyState iinfo state
      withMay "Can't update policy state" (pure maybeIinfo) $ \iinfo' -> do
        let tx =
              mconcat
                [ spendBox tv r box
                , payToScript tv (InlineDatum iinfo') value
                ]
        submitTx pkh tx

cancelPolicy :: InsuranceId -> PubKeyHash -> Run ()
cancelPolicy = runUpdatePolicyState Cancelled (PolicyInitiated PolicyInitiatedCancel)

testCancelPolicyIfIllegalState :: InsurancePolicyState -> Run ()
testCancelPolicyIfIllegalState state = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  runUpdatePolicyState state (PolicyInitiated PolicyInitiatedCancel) iid broker1

testCancelPolicyByBroker :: Run ()
testCancelPolicyByBroker = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  cancelPolicy iid broker1

testCancelPolicyByUnauthorizedUser :: Run ()
testCancelPolicyByUnauthorizedUser = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  cancelPolicy iid investor1

iinfoBox :: InsuranceId -> TxBox script -> Bool
iinfoBox (InsuranceId cs) (TxBox _ (TxOut _ value _ _) _) =
  valueOf value cs policyInfoTokenName == 1

newSamplePolicy :: Users -> Run InsuranceId
newSamplePolicy Users {..} = do
  let icpPolicyHolder = policyHolder
      icpFidaCardQuantity = 10
      icpPolicyPrice = icpFidaCardQuantity * 4 * 5 * 1_000_000
      icpFidaCardValue = 1_000 * 1_000_000
      icpPolicyAuthority = AtLeastOneSign [fidaSystem, broker1]
  makePolicy broker1 InsuranceCreateParams {..}

data Users = Users
  { fidaSystem :: PubKeyHash
  , broker1 :: PubKeyHash
  , broker2 :: PubKeyHash
  , policyHolder :: PubKeyHash
  , investor1 :: PubKeyHash
  , investor2 :: PubKeyHash
  , investor3 :: PubKeyHash
  }
  deriving (Show)

type InsurancePolicy = TypedValidator InsurancePolicyDatum InsurancePolicyRedeemer

type PiggyBank = TypedValidator PiggyBankDatum PiggyBankRedeemer

data InsuranceCreateParams = InsuranceCreateParams
  { icpPolicyHolder :: PubKeyHash
  , icpPolicyPrice :: Integer
  , icpFidaCardQuantity :: Integer
  , icpFidaCardValue :: Integer
  , icpPolicyAuthority :: InsuranceAuthority
  }

insurancePolicy :: InsuranceId -> InsurancePolicy
insurancePolicy = TypedValidator . toV2 . insurancePolicyValidator

piggyBank :: InsuranceId -> FidaCardId -> PiggyBank
piggyBank iid = TypedValidator . toV2 . piggyBankValidator iid

piggyBankAddr :: InsuranceId -> FidaCardId -> Address
piggyBankAddr iid = toAddress . piggyBank iid

insuranceIdNFT :: TxOutRef -> TypedPolicy ()
insuranceIdNFT = TypedPolicy . toV2 . insuranceIdMintingPolicy

setupUsers :: Run Users
setupUsers = do
  users <- replicateM 7 $ newUser $ adaValue 100_000_000
  let [fidaSystem, broker1, broker2, policyHolder, investor1, investor2, investor3] = users
  writeUserName fidaSystem "Fida System Wallet"
  writeUserName broker1 "Policy Broker 1"
  writeUserName broker2 "Policy Broker 2"
  writeUserName policyHolder "Policy Holder"
  writeUserName investor1 "Investor 1"
  writeUserName investor2 "Investor 2"
  writeUserName investor3 "Investor 3"
  return Users {..}

fidaCardFromInt :: Integer -> FidaCardId
fidaCardFromInt = FidaCardId . stringToBuiltinByteString . show

iinfoFromParams :: InsuranceCreateParams -> InsurancePolicyDatum
iinfoFromParams InsuranceCreateParams {..} =
  let iInfoClaim = Nothing
      iInfoStartDate = Nothing
      iInfoCollateralAmount = icpFidaCardQuantity * icpFidaCardValue
      iInfoFidaCardValue = icpFidaCardValue
      iInfoFidaCardNumber = icpFidaCardQuantity
      iInfoPremiumAmount = icpPolicyPrice
      iInfoInstallments =
        InstallmentsInfo
          [ days 90
          , days 90
          , days 90
          ]
          (iInfoPremiumAmount `div` iInfoFidaCardNumber `div` 4)
      iInfoInsurancePeriod = days 365
      iInfoPolicyHolder = icpPolicyHolder
      iInfoPolicyAuthority = icpPolicyAuthority
      iInfoState = Initiated
      iInfoClaimTimeToLive = days 7
      iInfoFundingDeadline = beginningOfTime + fromMilliSeconds (days 7)
      iInfoTotalClaimsAcceptedAmount = 0
      iInfoClaimTimeToPay = days 7
   in InsuranceInfo {..}

makePolicy :: PubKeyHash -> InsuranceCreateParams -> Run InsuranceId
makePolicy broker params@InsuranceCreateParams {..} = do
  utxos <- utxoAt broker
  sp <- spend broker $ scale (icpFidaCardQuantity + 2) oneAda
  let [(ref, _)] = utxos
      insuranceIdNFTScript = insuranceIdNFT ref
      iid = InsuranceId $ scriptCurrencySymbol insuranceIdNFTScript
      insuranceScript = insurancePolicy iid
      tx =
        mconcat $
          [ payToScript insuranceScript (InlineDatum iinfo) (oneAda <> policyInfoNFT iid)
          , payToScript insuranceScript (InlineDatum $ paymentInfo iid) (oneAda <> policyPaymentNFT iid)
          , spendPubKey ref
          , userSpend sp
          , mintValue insuranceIdNFTScript () (policyInfoNFT iid <> policyPaymentNFT iid)
          ]
            <> payToPgiggyBanks insuranceIdNFTScript iid
  submitTx broker tx
  return iid
 where
  oneAda = adaValue 1

  iinfo = iinfoFromParams params

  payToPgiggyBanks script iid =
    map (payToPiggyBank script iid . fidaCardFromInt) [1 .. icpFidaCardQuantity]

  fidaCardDatum fcid =
    let pbfcIsSold = False
        pbfcFidaCardId = fcid
        pbfcFidaCardValue = icpFidaCardValue
        pbfcPaidClaims = []
     in PBankFidaCard {..}

  payToPiggyBank insuranceIdNFTScript iid fcid =
    let script = piggyBank iid fcid
        value = fidaCardStatusNFT iid <> fidaCardNFT iid fcid
     in mconcat
          [ payToScript script (InlineDatum $ fidaCardDatum fcid) (oneAda <> value)
          , mintValue insuranceIdNFTScript () value
          ]

  paymentInfo iid =
    let ppInfoPremiumAmountPerPiggyBank = icpPolicyPrice `div` icpFidaCardQuantity
        ppInfoPiggyBanks = map (piggyBankAddr iid . fidaCardFromInt) [1 .. icpFidaCardQuantity]
     in PremiumPaymentInfo {..}

policyInfoNFT :: InsuranceId -> Value
policyInfoNFT (InsuranceId cs) = singleton cs policyInfoTokenName 1

policyPaymentNFT :: InsuranceId -> Value
policyPaymentNFT (InsuranceId cs) = singleton cs policyPaymentTokenName 1

fidaCardNFT :: InsuranceId -> FidaCardId -> Value
fidaCardNFT (InsuranceId cs) (FidaCardId fcid) = singleton cs (fidaCardTokenName fcid) 1

fidaCardStatusNFT :: InsuranceId -> Value
fidaCardStatusNFT (InsuranceId cs) = singleton cs fidaCardStatusTokenName 1

-- | Friday, 1 March 2024 12:12:12
beginningOfTime :: POSIXTime
beginningOfTime = POSIXTime 1709295132000

days :: Integer -> DiffMilliSeconds
days n = DiffMilliSeconds $ 1000 * 60 * 60 * 24 * n
