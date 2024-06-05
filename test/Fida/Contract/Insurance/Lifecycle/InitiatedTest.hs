{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.InitiatedTest (tests) where

import Control.Monad (void)
import Fida.Contract.Insurance.Datum
  ( InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..), PolicyInitiatedRedemeer (..))
import Fida.Contract.TestToolbox
  ( Run,
    Users (..),
    bad,
    good,
    insurancePolicy,
    newSamplePolicy,
    runUpdatePolicyState,
    setupUsers,
  )
import Plutus.Model
import Plutus.V2.Ledger.Api (PubKeyHash, TxOut (..))
import Test.Tasty (TestTree, testGroup)
import Prelude

import Fida.Contract.TestToolbox
import Plutus.V2.Ledger.Api

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
    , good "Paying premium works" testPayPremium
    ]

testCreatePolicy :: Run ()
testCreatePolicy = void $ setupUsers >>= newSamplePolicy

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

payPremiumToPiggyBanks ::
  InsurancePolicy ->
  TxBox InsurancePolicy ->
  PubKeyHash ->
  Maybe Tx
payPremiumToPiggyBanks tv box@(TxBox _ (TxOut _ value _ _) ppinfo@PremiumPaymentInfo {..}) pkh =
  Just $ mconcat (payToPiggyBankTx <$> ppInfoPiggyBanks) <> spendPPaymentInfo
 where
  r = PolicyInitiated PolicyInitiatedPayPremium

  spendPPaymentInfo =
    mconcat
      [ spendBox tv r box
      , payToKey pkh value
      ]

  datum = InlineDatum $ PBankPremium ppInfoPremiumAmountPerPiggyBank

  payToPiggyBankTx addr =
    payToAddressDatum addr datum (adaValue ppInfoPremiumAmountPerPiggyBank)
payPremiumToPiggyBanks _ _ _ = Nothing

testPayPremium :: Run ()
testPayPremium = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  sp <- spend policyHolder $ adaValue 200_000_000
  let tv = insurancePolicy iid
  withBox @InsurancePolicy (iinfoBox iid) tv $ \iiBox ->
    withBox @InsurancePolicy (ppInfoBox iid) tv $ \piBox -> do
      let r = PolicyInitiated PolicyInitiatedPayPremium
          maybeUpdateStTx = updatePolicyStateTx tv iiBox Funding r
          maybePayToPiggyBanksTx = payPremiumToPiggyBanks tv piBox policyHolder
          maybePayPremiumTx = (<>) <$> maybeUpdateStTx <*> maybePayToPiggyBanksTx
      withMay "Can't update policy state" (pure maybePayPremiumTx) $ \payPremiumTx -> do
        let tx =
              mconcat
                [ payPremiumTx
                , userSpend sp
                ]
        submitTx policyHolder tx
