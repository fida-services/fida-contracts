module Fida.Contract.TestToolbox.Utils
    (isScriptRef
    ) where


import Plutus.Model
import Plutus.V2.Ledger.Api
import Prelude

isScriptRef :: HasValidatorHash script => script -> (TxOutRef, TxOut) -> Bool
isScriptRef script (ref, TxOut _ _ _ (Just (ScriptHash hash))) =
  let ValidatorHash hash' = toValidatorHash script
   in hash' == hash