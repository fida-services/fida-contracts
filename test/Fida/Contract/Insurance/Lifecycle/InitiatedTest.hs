module Fida.Contract.Insurance.Lifecycle.InitiatedTest (tests) where

import Control.Monad (mapM, replicateM)
import Data.List
import Fida.Contract.Insurance
import Fida.Contract.Insurance.Authority
import Fida.Contract.Insurance.Datum
import Fida.Contract.Insurance.InsuranceId
import Fida.Contract.Insurance.Redeemer
import Plutus.Model
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude (Bool (..), Eq ((==)), Integer, return, ($), (&&), (.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude (undefined)

tests :: TestTree
tests =
    testGroup
        "Unit tests for Insurance.Lifecycle.Initiated module"
        [ good "User can exchange values" userExchange
        ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000_000) defaultBabbage

type InsurancePolicy = TypedValidator InsurancePolicyDatum InsurancePolicyRedeemer

data InsuranceCreateParams = InsuranceCreateParams
    { icpPolicyHolder :: PubKeyHash
    , icpPolicyPrice :: Integer
    , icpFidaCardQuantity :: Integer
    , icpFidaCardValue :: Integer
    , icpPolicyAuthority :: InsuranceAuthority
    }

insurancePolicy :: InsuranceId -> InsurancePolicy
insurancePolicy = TypedValidator . toV2 . insurancePolicyValidator

insuranceIdNFT :: TxOutRef -> TypedPolicy ()
insuranceIdNFT = TypedPolicy . toV2 . insuranceIdMintingPolicy

userExchange :: Run ()
userExchange = do
    users <- setupUsers
    let [u1, u2, u3] = users
    sendValue u1 (adaValue 100) u2
    sendValue u2 (adaValue 100) u3

setupUsers :: Run [PubKeyHash]
setupUsers = do
    users@[u1, u2, u3, u4, u5] <- replicateM 5 $ newUser $ adaValue 100_000_000
    writeUserName u1 "Policy Broker"
    writeUserName u2 "Policy Holder"
    writeUserName u3 "Investor 1"
    writeUserName u4 "Investor 2"
    writeUserName u5 "Investor 3"
    return users

createPolicy :: PubKeyHash -> InsuranceCreateParams -> Run ()
createPolicy pkh datum = do
    return ()
  where
    paymentInfo :: InsuranceId -> Integer -> [Address] -> InsurancePolicyDatum
    paymentInfo = undefined
