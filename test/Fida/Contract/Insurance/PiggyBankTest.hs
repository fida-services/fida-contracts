{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.Insurance.PiggyBankTest (tests) where

import Fida.Contract.TestToolbox
  ( Run,
    Users (..),
    bad,
    fidaCardsFromInts,
    fidaCardFromInt,
    good,
    insurancePolicy,
    newSamplePolicy,
    runUpdatePolicyState,
    setupUsers,
    triggerFundingComplete,
    triggerPolicyExpiration,
    unlockCollateralsOnExpired,
    unlockCollateralOnExpired,
    unlockCollateralOnCancel,
    payPremium,
    claimPremium,
    claimPremiumOnCancel
  )
import Fida.Contract.Insurance.Datum (InsurancePolicyState (..))
import Fida.Contract.Insurance.Redeemer (InsurancePolicyRedeemer (..),
                                         PolicyOnRiskRedeemer (..))
import Fida.Contract.Insurance.Lifecycle.OnRiskTest (createClaim, acceptClaim)
import Fida.Contract.TestToolbox.Action (buyFidaCards,
                                         sellFidaCard,
                                         payForClaimWithCollateral)
import Fida.Contract.TestToolbox.TypedValidators (PiggyBank)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.Model
import Plutus.Model.Ada
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unit tests for PiggyBank module"
    [ good "Buying Fida card works" testBuyFidaCard
    , good "Sell Fida card works" testSellFidaCard
    , good "Claim premium works" testClaimPremium
    , good "Claim premium on cancel works" testClaimPremiumOnCancel
    , good "Pay for claim with collaterl works" testPayForClaimWithCollateral
    , good "Unlock collateral on cancel works" testUnlockCollateralOnCancel
    , good "Unlock collateral works" testUnlockCollateral
    ]


testBuyFidaCard :: Run ()
testBuyFidaCard = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users
  buyFidaCards iid investor1 $ fidaCardsFromInts [1 .. 5]


testSellFidaCard :: Run ()
testSellFidaCard = do
  users@Users {..} <- setupUsers
  
  iid <- newSamplePolicy users
  
  buyFidaCards iid investor1 $ fidaCardsFromInts [1 .. 3]
  
  sellFidaCard iid investor1 $ fidaCardFromInt 1


testClaimPremium :: Run ()
testClaimPremium = do
  users@Users {..} <- setupUsers
  
  iid <- newSamplePolicy users

  payPremium iid policyHolder

  let fidaCards = fidaCardsFromInts [1 .. 10]

  buyFidaCards iid investor1 fidaCards
  
  triggerFundingComplete iid users

  time <- currentTime

  waitUntil $ time + days 90

  claimPremium iid (head fidaCards) investor1 (asAda 5)


testPayForClaimWithCollateral :: Run ()
testPayForClaimWithCollateral = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

  acceptClaim iid fidaSystem users

  payForClaimWithCollateral iid investor1


testClaimPremiumOnCancel :: Run ()
testClaimPremiumOnCancel = do
  users@Users {..} <- setupUsers
  
  iid <- newSamplePolicy users

  payPremium iid policyHolder

  let fidaCards = fidaCardsFromInts [1 .. 10]

  buyFidaCards iid investor1 fidaCards
  
  triggerFundingComplete iid users

  time <- currentTime

  waitUntil $ time + days 90

  runUpdatePolicyState Cancelled (PolicyOnRisk PolicyOnRiskCancel) iid broker1

  claimPremium iid (head fidaCards) investor1 (asAda 5)
  
  claimPremiumOnCancel iid (head fidaCards) policyHolder (asAda 15)


testUnlockCollateralOnCancel :: Run ()
testUnlockCollateralOnCancel = do
  users@Users {..} <- setupUsers

  iid <- newSamplePolicy users

  payPremium iid policyHolder

  buyFidaCards iid investor1 $ [fidaCardFromInt 1]
  
  buyFidaCards iid investor2 $ [fidaCardFromInt 2]

  runUpdatePolicyState Cancelled (PolicyOnRisk PolicyOnRiskCancel) iid broker1

  unlockCollateralOnCancel investor1 iid $ fidaCardFromInt 1
  
  unlockCollateralOnCancel investor2 iid $ fidaCardFromInt 2


testUnlockCollateral :: Run ()
testUnlockCollateral = do
  users@Users {..} <- setupUsers
  
  iid <- newSamplePolicy users

  payPremium iid policyHolder

  let fidaCards = fidaCardsFromInts [1 .. 10]

  buyFidaCards iid investor1 fidaCards
  
  triggerFundingComplete iid users

  time <- currentTime

  waitUntil $ time + days 366

  triggerPolicyExpiration iid broker1
  
  unlockCollateralsOnExpired investor1 iid fidaCards 
