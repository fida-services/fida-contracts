{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Fida.Contract.Insurance.Datum
  ( InsurancePolicyState (..),
    InsurancePolicyDatum (..),
    PiggyBankDatum (..),
    ClaimInfo (..),
    FidaCardId (..),
    InstallmentsInfo (..),
    updatePolicyState,
    completeFunding,
    untypedUpdatePolicyState,
    updateClaim,
    untypedUnsetClaim,
    untypedUpdateClaim,
    unlockedPremiumToClaim,
    setFidaCardSold,
    setFidaCardUnsold,
    untypedSetFidaCardSold,
    addPiggyBankPaidClaim,
    updatePiggyBankRefund,
    untypedUpdatePiggyBankRefund,
  )
where

import Fida.Contract.Insurance.Authority (InsuranceAuthority)
import qualified Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Time (DiffMilliSeconds, fromMilliSeconds)
import Plutus.V2.Ledger.Api
  ( Address,
    FromData,
    POSIXTime,
    POSIXTimeRange,
    PubKeyHash,
    ToData,
    UnsafeFromData,
  )
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as HPrelude

newtype FidaCardId = FidaCardId BuiltinByteString
  deriving newtype (ToData, FromData, UnsafeFromData, HPrelude.Show, HPrelude.Eq)

PlutusTx.makeLift ''FidaCardId

type PremiumAmount = Integer

data InsurancePolicyState
  = Initiated
  | Funding
  | OnRisk
  | Cancelled
  | Expired
  deriving (HPrelude.Show, HPrelude.Eq)

instance Eq InsurancePolicyState where
  {-# INLINEABLE (==) #-}
  Initiated == Initiated = True
  Funding == Funding = True
  OnRisk == OnRisk = True
  Cancelled == Cancelled = True
  Expired == Expired = True
  _ == _ = False

PlutusTx.makeIsDataIndexed
  ''InsurancePolicyState
  [ ('Initiated, 0)
  , ('Funding, 1)
  , ('OnRisk, 2)
  , ('Cancelled, 3)
  , ('Expired, 4)
  ]

PlutusTx.makeLift ''InsurancePolicyState

data InstallmentsInfo
  = InstallmentsInfo
      [DiffMilliSeconds] -- payment intervals
      PremiumAmount -- how much to pay per installment (for one fida card)
  deriving (HPrelude.Show,  HPrelude.Eq)

PlutusTx.makeIsDataIndexed
  ''InstallmentsInfo
  [ ('InstallmentsInfo, 0)
  ]

PlutusTx.makeLift ''InstallmentsInfo

data ClaimInfo = ClaimInfo
  { claimAmount :: Integer
  , claimDate :: POSIXTime
  , claimReason :: BuiltinByteString
  , claimAccepted :: Bool
  , claimId :: BuiltinByteString
  }
  deriving (HPrelude.Show, HPrelude.Eq)

PlutusTx.makeIsDataIndexed
  ''ClaimInfo
  [ ('ClaimInfo, 0)
  ]

PlutusTx.makeLift ''ClaimInfo

data InsurancePolicyDatum
  = InsuranceInfo
      { iInfoCollateralAmount :: Integer
      , iInfoFidaCardValue :: Integer
      , iInfoFidaCardNumber :: Integer
      , iInfoPremiumAmount :: Integer
      , iInfoInstallments :: InstallmentsInfo
      , iInfoInsurancePeriod :: DiffMilliSeconds
      , iInfoPolicyHolder :: PubKeyHash
      , iInfoPolicyAuthority :: InsuranceAuthority
      , iInfoState :: InsurancePolicyState
      , iInfoFundingDeadline :: POSIXTime
      , iInfoTotalClaimsAcceptedAmount :: Integer
      , iInfoClaimTimeToLive :: DiffMilliSeconds
      , iInfoClaimTimeToPay :: DiffMilliSeconds
      , iInfoStartDate :: Maybe POSIXTime
      , iInfoClaim :: Maybe ClaimInfo
      }
  | PremiumPaymentInfo
      { -- | in lovelace
        ppInfoPremiumAmountPerPiggyBank :: Integer
      , ppInfoPiggyBanks :: [Address]
      }
  | PolicyClaimPayment
  deriving (HPrelude.Show, HPrelude.Eq)

{-# INLINEABLE updatePolicyState #-}
updatePolicyState :: InsurancePolicyDatum -> InsurancePolicyState -> Maybe InsurancePolicyDatum
updatePolicyState InsuranceInfo {..} state = Just $ InsuranceInfo {iInfoState = state, ..}
updatePolicyState _ _ = Nothing

{-# INLINEABLE untypedUpdatePolicyState #-}
untypedUpdatePolicyState :: InsurancePolicyDatum -> InsurancePolicyState -> Maybe BuiltinData
untypedUpdatePolicyState d = fmap PlutusTx.toBuiltinData . updatePolicyState d

{-# INLINEABLE completeFunding #-}
completeFunding :: InsurancePolicyDatum -> POSIXTime -> Maybe InsurancePolicyDatum
completeFunding InsuranceInfo {..} startDate = Just $ InsuranceInfo {iInfoState = OnRisk, iInfoStartDate = Just startDate, ..}
completeFunding _ _ = Nothing

{-# INLINEABLE updateClaim #-}
updateClaim :: InsurancePolicyDatum -> Maybe ClaimInfo -> Maybe InsurancePolicyDatum
updateClaim InsuranceInfo {..} claim = Just $ InsuranceInfo {iInfoClaim = claim, ..}
updateClaim _ _ = Nothing

{-# INLINEABLE untypedUpdateClaim #-}
untypedUpdateClaim :: InsurancePolicyDatum -> ClaimInfo -> Maybe BuiltinData
untypedUpdateClaim d = fmap PlutusTx.toBuiltinData . updateClaim d . Just

{-# INLINEABLE untypedUnsetClaim #-}
untypedUnsetClaim :: InsurancePolicyDatum -> Maybe BuiltinData
untypedUnsetClaim d = fmap PlutusTx.toBuiltinData . updateClaim d $ Nothing

PlutusTx.makeIsDataIndexed
  ''InsurancePolicyDatum
  [ ('InsuranceInfo, 0)
  , ('PremiumPaymentInfo, 1)
  , ('PolicyClaimPayment, 2)
  ]

PlutusTx.makeLift ''InsurancePolicyDatum

data PiggyBankDatum
  = PBankPremium
      { pbankPremium'init :: PremiumAmount
      , pbankPremium'refund :: PremiumAmount
      }
  | PBankFidaCard
      { pbfcIsSold :: Bool
      , pbfcFidaCardId :: FidaCardId
      , pbfcFidaCardValue :: Integer
      , pbfcPaidClaims :: [BuiltinByteString]
      }
  deriving (HPrelude.Show)

{-# INLINEABLE updatePiggyBankRefund #-}
updatePiggyBankRefund :: PremiumAmount -> PiggyBankDatum -> Maybe PiggyBankDatum
updatePiggyBankRefund amount PBankPremium {..} = Just $ PBankPremium {pbankPremium'refund = amount, ..}
updatePiggyBankRefund _ _ = Nothing

{-# INLINEABLE untypedUpdatePiggyBankRefund #-}
untypedUpdatePiggyBankRefund :: PremiumAmount -> PiggyBankDatum -> Maybe BuiltinData
untypedUpdatePiggyBankRefund amount = fmap PlutusTx.toBuiltinData . updatePiggyBankRefund amount

{-# INLINEABLE addPiggyBankPaidClaim #-}
addPiggyBankPaidClaim :: BuiltinByteString -> PiggyBankDatum -> Maybe PiggyBankDatum
addPiggyBankPaidClaim claimId PBankFidaCard {..} = Just $ PBankFidaCard {pbfcPaidClaims = claimId : pbfcPaidClaims, ..}
addPiggyBankPaidClaim _ _ = Nothing

{-# INLINEABLE updatePiggyBankSoldFidaCardStatus #-}
updatePiggyBankSoldFidaCardStatus :: Bool -> PiggyBankDatum -> Maybe PiggyBankDatum
updatePiggyBankSoldFidaCardStatus status PBankFidaCard {..} = Just $ PBankFidaCard {pbfcIsSold = status, ..}
updatePiggyBankSoldFidaCardStatus _ _ = Nothing

{-# INLINEABLE setFidaCardSold #-}
setFidaCardSold :: PiggyBankDatum -> Maybe PiggyBankDatum
setFidaCardSold = updatePiggyBankSoldFidaCardStatus True

{-# INLINEABLE setFidaCardUnsold #-}
setFidaCardUnsold :: PiggyBankDatum -> Maybe PiggyBankDatum
setFidaCardUnsold = updatePiggyBankSoldFidaCardStatus False

{-# INLINEABLE untypedSetFidaCardSold #-}
untypedSetFidaCardSold :: PiggyBankDatum -> Maybe BuiltinData
untypedSetFidaCardSold = fmap PlutusTx.toBuiltinData . setFidaCardSold

PlutusTx.makeIsDataIndexed
  ''PiggyBankDatum
  [ ('PBankPremium, 0)
  , ('PBankFidaCard, 1)
  ]

{-# INLINEABLE unlockedPremiumToClaim #-}
unlockedPremiumToClaim ::
  POSIXTimeRange -> -- current time
  PremiumAmount -> -- locked premium amount (initial amount)
  InstallmentsInfo -> -- payment intervals
  POSIXTime -> -- insurance policy start date
  PremiumAmount
unlockedPremiumToClaim range locked (InstallmentsInfo intervals dpa) start = go 0 start intervals
 where
  go _ _ [] = locked
  go unlocked date (dt : rest) =
    let nextDate = fromMilliSeconds dt + date
     in if (Interval.before nextDate range)
          then go (unlocked + dpa) nextDate rest
          else unlocked
