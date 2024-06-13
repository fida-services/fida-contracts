{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.OnRiskTest (tests) where

import Fida.Contract.Insurance.Lifecycle.FundingTest (fundingComplete)
import Control.Monad (void)
import Fida.Contract.Insurance.Datum
  ( InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
    ClaimInfo (..),
    updateClaim,
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId)
import Fida.Contract.Insurance.Lifecycle.InitiatedTest (testPayPremium)
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..), PolicyOnRiskRedeemer (..), PolicyInitiatedRedemeer (..))
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
  )
import Plutus.Model
import Plutus.V2.Ledger.Api (PubKeyHash, TxOut (..))
import Test.Tasty (TestTree, testGroup)
import Prelude

import Fida.Contract.TestToolbox hiding (seconds)
import Plutus.V2.Ledger.Api

tests :: TestTree
tests =
  testGroup
    "Unit tests for Lifecycle.OnRisk module"
    [ good "Policy holder can create claim" testCreateClaim
    , good "Policy holder can close claim" testCloseClaim
    , good "Fida System can accept claim" testAcceptClaim
    , good "Investor can expire claim" testExpireClaim
    , good "Fida System can fail claim" testFailClaim
    ]

testCreateClaim :: Run ()
testCreateClaim = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

createClaim :: InsuranceId -> PubKeyHash -> Users -> Run ()
createClaim iid policyHolder users = do

  fundingComplete iid policyHolder users

  claimDate <- currentTime
  waitNSlots 5

  let claim = ClaimInfo
        { claimAccepted = False
        , claimDate = claimDate
        , claimAmount = 10_000_000
        , claimReason = "I'm sick!"
        , claimId = "1234"
        }

  let redeemer = PolicyOnRisk $ PolicyOnRiskCreateClaim claim

  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $ \box@(TxBox _ (TxOut _ value _ _) iinfoDatum) -> do
    validRangeStart <- currentTime
    let newDatum = updateClaim iinfoDatum (Just claim)
    withMay "Can't create claim" (pure newDatum) $ \datum -> do
      let tx = mconcat
                [ spendBox tv redeemer box
                , payToScript tv (InlineDatum datum) value
                ]

      tx' <- validateIn (from validRangeStart) tx
      submitTx policyHolder tx'
      return ()

testCloseClaim :: Run ()
testCloseClaim = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

  closeClaim iid policyHolder users

closeClaim :: InsuranceId -> PubKeyHash -> Users -> Run ()
closeClaim iid policyHolder users = do

  let redeemer = PolicyOnRisk $ PolicyOnRiskCloseClaim

  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $ \box@(TxBox _ (TxOut _ value _ _) iinfoDatum) -> do
    let newDatum = updateClaim iinfoDatum Nothing
    withMay "Can't close claim" (pure newDatum) $ \datum -> do
      let tx = mconcat
                [ spendBox tv redeemer box
                , payToScript tv (InlineDatum datum) value
                ]

      submitTx policyHolder tx
      return ()

testAcceptClaim :: Run ()
testAcceptClaim = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

  acceptClaim iid fidaSystem users

acceptClaim :: InsuranceId -> PubKeyHash -> Users -> Run ()
acceptClaim iid fidaSystem users = do

  let redeemer = PolicyOnRisk $ PolicyOnRiskAcceptClaim

  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $ \box@(TxBox _ (TxOut _ value _ _) iinfoDatum@InsuranceInfo{iInfoClaim = claim}) -> do
    let newDatum = updateClaim iinfoDatum $ ((\c -> c{claimAccepted=True}) <$> claim)
    withMay "Can't accept claim" (pure newDatum) $ \datum -> do
      let tx = mconcat
                [ spendBox tv redeemer box
                , payToScript tv (InlineDatum datum) value
                ]

      submitTx fidaSystem tx
      return ()

testExpireClaim :: Run ()
testExpireClaim = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

  waitNSlots 100

  expireClaim iid investor1 users

expireClaim :: InsuranceId -> PubKeyHash -> Users -> Run ()
expireClaim iid investor1 users = do

  let redeemer = PolicyOnRisk $ PolicyOnRiskExpireClaim

  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $ \box@(TxBox _ (TxOut _ value _ _) iinfoDatum) -> do
    let newDatum = updateClaim iinfoDatum Nothing
    withMay "Can't expire claim" (pure newDatum) $ \datum -> do
      let tx = mconcat
                [ spendBox tv redeemer box
                , payToScript tv (InlineDatum datum) value
                ]
      validRangeStart <- currentTime
      tx' <- validateIn (from validRangeStart) tx
      submitTx investor1 tx'
      return ()

testFailClaim :: Run ()
testFailClaim = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

  waitNSlots 100

  failClaim iid fidaSystem users

failClaim :: InsuranceId -> PubKeyHash -> Users -> Run ()
failClaim iid fidaSystem users = do

  let redeemer = PolicyOnRisk $ PolicyOnRiskFailClaim

  let tv = insurancePolicy iid
  withBox @ InsurancePolicy (iinfoBox iid) tv $ \box@(TxBox _ (TxOut _ value _ _) iinfoDatum) -> do
    let newDatum = updateClaim iinfoDatum Nothing
    withMay "Can't fail claim" (pure newDatum) $ \datum -> do
      let tx = mconcat
                [ spendBox tv redeemer box
                , payToScript tv (InlineDatum datum) value
                ]
      submitTx fidaSystem tx
      return ()