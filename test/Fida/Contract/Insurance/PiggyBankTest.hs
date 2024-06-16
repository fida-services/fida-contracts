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
    unlockCollaterals,
    payPremium
  )
import Fida.Contract.Insurance.Lifecycle.OnRiskTest (createClaim, acceptClaim)
import Fida.Contract.TestToolbox.Action (buyFidaCards,
                                         sellFidaCard,
                                         payForClaimWithCollateral)
import Fida.Contract.TestToolbox.TypedValidators (PiggyBank)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.Model
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unit tests for PiggyBank module"
    [ good "Buying Fida card works" testBuyFidaCard
    , good "Sell Fida card works" testSellFidaCard
--    , good "Claim premium works" testClaimPremium
--    , good "Claim premium on cancel works" testClaimPremiumOnCancel
    , good "Pay for claim with collaterl works" testPayForClaimWithCollateral
--    , good "Unlock collateral on cancel works" testUnlockCollateralOnCancel
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
  return ()

testClaimPremium :: Run ()
testClaimPremium = undefined

testPayForClaimWithCollateral :: Run ()
testPayForClaimWithCollateral = do
  users@Users {..} <- setupUsers
  iid <- newSamplePolicy users

  createClaim iid policyHolder users

  acceptClaim iid fidaSystem users

  payForClaimWithCollateral iid investor1

testClaimPremiumOnCancel :: Run ()
testClaimPremiumOnCancel = undefined

testUnlockCollateralOnCancel :: Run ()
testUnlockCollateralOnCancel = undefined

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
  
  unlockCollaterals investor1 iid fidaCards 
