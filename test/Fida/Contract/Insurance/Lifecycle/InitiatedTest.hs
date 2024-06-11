{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.Lifecycle.InitiatedTest (tests, testPayPremium) where

import Control.Monad (forM_, void)
import Data.Maybe (isJust)
import Fida.Contract.Insurance.Datum
  ( FidaCardId (..),
    InsurancePolicyDatum (..),
    InsurancePolicyState (..),
    PiggyBankDatum (..),
  )
import Fida.Contract.Insurance.InsuranceId (InsuranceId (..))
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..), PolicyInitiatedRedemeer (..))
import Fida.Contract.Insurance.Tokens (fidaCardTokenName, policyInfoTokenName, policyPaymentTokenName)
import Fida.Contract.TestToolbox
  ( InsurancePolicy,
    PiggyBank,
    Run,
    Users (..),
    assertTrue,
    bad,
    fidaCardFromInt,
    good,
    iinfoBox,
    insurancePolicy,
    isScriptRef,
    newSamplePolicy,
    payPremium,
    piggyBank,
    piggyBankAddr,
    piggyBankInfoBox,
    ppInfoBox,
    runUpdatePolicyState,
    setupUsers,
    updatePolicyStateTxRef,
  )
import Plutus.Model
  ( TxBox (..),
    adaValue,
    logError,
    spend,
    submitTx,
    userSpend,
    withBox,
    withMay,
    withRefScript,
  )
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (PubKeyHash, TxOut (..))
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
    , good "Paying premium works" testPayPremium
    , good "All required utxos are there" testRequiredUtxos
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

testPayPremium :: Run ()
testPayPremium = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  payPremium iid users

testRequiredUtxos :: Run ()
testRequiredUtxos = do
  users <- setupUsers
  iid@(InsuranceId cs) <- newSamplePolicy users
  let tv = insurancePolicy iid
  withBox @InsurancePolicy (iinfoBox iid) tv $ \(TxBox _ (TxOut _ iinfoValue _ _) iinfo) ->
    withBox @InsurancePolicy (ppInfoBox iid) tv $ \(TxBox _ (TxOut _ ppinfoValue _ _) ppinfo) -> do
      case (iinfo, ppinfo) of
        (InsuranceInfo {..}, PremiumPaymentInfo {..}) -> do
          assertTrue "No insurance info token" $ valueOf iinfoValue cs policyInfoTokenName == 1
          assertTrue "No insurance payment info token" $ valueOf ppinfoValue cs policyPaymentTokenName == 1
          assertTrue "Fida card value doesn't match" $ iInfoFidaCardValue == 1_000_000_000
          assertTrue "Collateral amount doesn't match" $ iInfoCollateralAmount == 10_000_000_000
          assertTrue "Fida card quantity doesn't match" $ iInfoFidaCardNumber == 10
          assertTrue "Premium amount doesn't match" $ iInfoPremiumAmount == 200_000_000
          assertTrue "Start date must be not set" $ not $ isJust iInfoStartDate
          assertTrue "Claim must be not set" $ not $ isJust iInfoClaim
          assertTrue "State must be set to Initiated" $ iInfoState == Initiated

          assertTrue "Premium amount per piggy bank doesn't match" $ ppInfoPremiumAmountPerPiggyBank == 20_000_000
          assertTrue "Piggy banks addresses don't match" $
            ppInfoPiggyBanks == map (piggyBankAddr iid . fidaCardFromInt) [1 .. 10]

          forM_ (map fidaCardFromInt [1 .. 10]) $ \fcid@(FidaCardId tn) ->
            let ptv = piggyBank iid fcid
             in withBox @PiggyBank (piggyBankInfoBox iid fcid) ptv $
                  \(TxBox _ (TxOut _ piggyBankInfoValue _ _) piggyBankDatum) -> do
                    let atpp msg = msg <> " @ piggy bank " <> show fcid
                    case piggyBankDatum of
                      PBankFidaCard {..} -> do
                        assertTrue "No piggy bank token" $ valueOf piggyBankInfoValue cs (fidaCardTokenName tn) == 1
                        assertTrue (atpp "Fida card id doesn't match") $ pbfcFidaCardId == fcid
                        assertTrue (atpp "Fida card value doesn't match") $ pbfcFidaCardValue == 1_000_000_000
                        assertTrue (atpp "Fida card must be available for selling") $ not pbfcIsSold
                        assertTrue (atpp "There should be no paid claims") $ null pbfcPaidClaims
                      _ -> logError "Datum for piggy bank dosen't match"
        _ -> logError "Datum types for insurance policy script don't match"
