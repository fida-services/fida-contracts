{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.FidaPolicyContractTest (tests) where

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
    payPremium,
    buyFidaCards
  )
import Plutus.Model ( currentTime,
                      waitUntil, days
                    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "Unit tests for FidaPolicyContract module"
    [ good "Expiration policy during Funding phase works" testPolicyExpirationDuringFunding
    , good "Expiration policy during OnRisk phase works" testPolicyExpirationDuringOnRisk
    ]


testPolicyExpirationDuringFunding :: Run ()
testPolicyExpirationDuringFunding = do
  users@Users {..} <- setupUsers

  iid <- newSamplePolicy users

  payPremium iid policyHolder

  time <- currentTime

  waitUntil $ time + days 7

  triggerPolicyExpiration iid broker1


testPolicyExpirationDuringOnRisk:: Run ()
testPolicyExpirationDuringOnRisk = do
  users@Users {..} <- setupUsers

  iid <- newSamplePolicy users

  payPremium iid policyHolder

  let fidaCards = fidaCardsFromInts [1 .. 10]

  buyFidaCards iid investor1 fidaCards

  triggerFundingComplete iid users

  time <- currentTime

  waitUntil $ time + days 366

  triggerPolicyExpiration iid broker1
