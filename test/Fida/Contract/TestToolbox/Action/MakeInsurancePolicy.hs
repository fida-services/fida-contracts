{-# LANGUAGE RecordWildCards #-}

module Fida.Contract.TestToolbox.Action.MakeInsurancePolicy
  ( InsuranceCreateParams (..),
    newSamplePolicy,
    makePolicy,
  )
where

import Fida.Contract.Insurance.Authority
import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId
import Fida.Contract.TestToolbox.Time (beginningOfTime, days, seconds)
import Fida.Contract.TestToolbox.TypedValidators
import Fida.Contract.TestToolbox.Users (Users (..))
import Plutus.Model hiding (days, seconds)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V1.Ledger.Value (scale)
import Plutus.V2.Ledger.Api
import Prelude

data InsuranceCreateParams = InsuranceCreateParams
  { icpPolicyHolder :: PubKeyHash
  , icpPolicyPrice :: Integer
  , icpFidaCardQuantity :: Integer
  , icpFidaCardValue :: Integer
  , icpPolicyAuthority :: InsuranceAuthority
  , icpFundingDeadline :: POSIXTime
  }

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
      iInfoClaimTimeToLive = seconds 100 --days 7
      iInfoFundingDeadline = icpFundingDeadline
      iInfoTotalClaimsAcceptedAmount = 0
      iInfoClaimTimeToPay = seconds 50 --days 7
   in InsuranceInfo {..}

newSamplePolicy :: Users -> Run InsuranceId
newSamplePolicy Users {..} = do
  now <- currentTime
  let icpPolicyHolder = policyHolder
      icpFidaCardQuantity = 10
      icpPolicyPrice = icpFidaCardQuantity * 4 * 5 * 1_000_000 -- 200_000_000
      icpFidaCardValue = 1_000 * 1_000_000
      icpPolicyAuthority = AtLeastOneSign [fidaSystem, broker1]
      icpFundingDeadline = now + fromMilliSeconds (days 7)
  makePolicy broker1 InsuranceCreateParams {..}

makePolicy :: PubKeyHash -> InsuranceCreateParams -> Run InsuranceId
makePolicy broker params@InsuranceCreateParams {..} = do
  --         policy info nft + policy payment nft  -|
  --                                                |
  sp <- spend broker $ scale (icpFidaCardQuantity + 2) oneAda
  let ref = getHeadRef sp
      insuranceIdNFTScript = insuranceIdNFT ref
      iid = InsuranceId $ scriptCurrencySymbol insuranceIdNFTScript
      insuranceScript = insurancePolicy iid
      tx =
        mconcat $
          [ payToRef insuranceScript (InlineDatum iinfo) (oneAda <> policyInfoNFT iid)
          , payToRef insuranceScript (InlineDatum $ paymentInfo iid) (oneAda <> policyPaymentNFT iid)
          , spendPubKey ref
          , userSpend sp
          , mintValue insuranceIdNFTScript () (policyInfoNFT iid <> policyPaymentNFT iid)
          ]
            <> payToPgiggyBanks insuranceIdNFTScript iid
  submitTx broker tx
  runLoadRefScript broker insuranceScript
  runLoadRefScript broker $ piggyBank iid
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
    let script = piggyBank iid
        value = fidaCardStatusNFT iid <> fidaCardNFT iid fcid
     in mconcat
          [ payToScript script (InlineDatum $ fidaCardDatum fcid) (oneAda <> value)
          , mintValue insuranceIdNFTScript () value
          ]

  paymentInfo iid =
    let ppInfoPremiumAmountPerPiggyBank = icpPolicyPrice `div` icpFidaCardQuantity
        ppInfoFidaCardIds = map fidaCardFromInt [1 .. icpFidaCardQuantity]
        ppInfoPiggyBankAddress = piggyBankAddr iid
     in PremiumPaymentInfo {..}
