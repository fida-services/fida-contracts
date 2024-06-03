{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.InitiatedTest (tests) where

import Fida.Contract.TestToolbox.Users (Users(..), setupUsers)
import Fida.Contract.TestToolbox.TypedValidators
import Fida.Contract.TestToolbox.Action.MakeInsurancePolicy (newSamplePolicy)
import Control.Monad (void)
import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId
import Fida.Contract.Insurance.Redeemer
import Plutus.Model hiding (days)
import Plutus.V2.Ledger.Api
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
