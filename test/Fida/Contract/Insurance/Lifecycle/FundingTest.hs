{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.FundingTest (tests)  where

import Control.Monad (void)
import Fida.Contract.Insurance.Datum
  ( InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Lifecycle.InitiatedTest (testPayPremium)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..), PolicyFundingRedeemer (..), PolicyInitiatedRedemeer (..))
import Fida.Contract.TestToolbox
  ( Run,
    Users (..),
    bad,
    completeFundingTx,
    good,
    insurancePolicy,
    newSamplePolicy,
    runUpdatePolicyState,
    setupUsers,
    triggerFundingComplete
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
    "Unit tests for Insurance.Lifecycle.Funding module"
    [ good "Broker is allowed to cancell a policy" testCancelPolicyByBroker
    , bad "Unauthorized user is not allowed to cancell a policy" testCancelPolicyByUnauthorizedUser
    , bad "Cancelling policy can't set state to Funding" $ testCancelPolicyIfIllegalState Funding
    , bad "Cancelling policy can't set state to OnRisk" $ testCancelPolicyIfIllegalState OnRisk
    , bad "Cancelling policy can't set state to Initiated" $ testCancelPolicyIfIllegalState Initiated
    , good "Completing funding phase works" $ testFundingComplete
    ]

testCancelPolicyIfIllegalState :: InsurancePolicyState -> Run ()
testCancelPolicyIfIllegalState state = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  runUpdatePolicyState state (PolicyInitiated PolicyInitiatedCancel) iid broker1

cancelPolicy :: InsuranceId -> PubKeyHash -> Run ()
cancelPolicy = runUpdatePolicyState Cancelled (PolicyInitiated PolicyInitiatedCancel)

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

testFundingComplete :: Run ()
testFundingComplete = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  payPremium iid policyHolder
  buyFidaCards iid investor1 $ fidaCardsFromInts [1 .. 10]
  triggerFundingComplete iid users
