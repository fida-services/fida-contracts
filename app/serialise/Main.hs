module Main (main) where

import Cardano.Api (PlutusScriptV2, serialiseToTextEnvelope, writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Short (toShort)
import Data.Foldable (for_)
import qualified Fida.Contract.Insurance as Insurance
import qualified Fida.Contract.Insurance.InsuranceId as Insurance
import qualified Fida.Contract.Insurance.PiggyBank as PiggyBank
import qualified Fida.Contract.Investor as Investor
import qualified Fida.Contract.SystemId as SystemId
import Options.Applicative
  ( Alternative ((<|>)),
    Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    short,
    strOption,
    (<**>),
  )
import qualified Plutonomy.UPLC
import Plutus.V2.Ledger.Api (Script)
import qualified System.IO as IO
import Prelude

data ClArgs
  = GeneratePlutusScipts
      { gpsOutDir :: !FilePath
      }
  | GeneratePursModule
      { gpmModuleName :: !String
      , gpmOutFile :: !(Maybe FilePath)
      }
  deriving (Show)

plutusScripts :: [([Char], Script)]
plutusScripts =
  [ ("InsuranceIdMintingPolicy", Insurance.serialisableInsuranceIdMintingPolicy)
  , ("InsurancePolicyValidator", Insurance.serialisableInsurancePolicyValidator)
  , ("InvestorValidator", Investor.serialisableInvestorValidator)
  , ("SystemIdMintingPolicy", SystemId.serialisableSystemIdMintingPolicy)
  , ("PiggyBankValidator", PiggyBank.serialisablePiggyBankValidator)
  ]

main :: IO ()
main = getArgs >>= app

app :: ClArgs -> IO ()
app (GeneratePlutusScipts outDir) =
  for_ plutusScripts $ \(name, script) -> serialiseScript outDir (name <> ".script") script
app (GeneratePursModule moduleName (Just outFile)) =
  IO.withFile outFile IO.ReadWriteMode $ \handle -> do
    IO.hSetFileSize handle 0
    serialiseScriptsToPurs moduleName plutusScripts handle
app (GeneratePursModule moduleName Nothing) =
  serialiseScriptsToPurs moduleName plutusScripts IO.stdout

getArgs :: IO ClArgs
getArgs = execParser opts
 where
  opts =
    info
      (genPlutusScript <|> genPursModule <**> helper)
      (fullDesc <> progDesc "Outputs plutus scripts")
  genPlutusScript :: Parser ClArgs
  genPlutusScript =
    GeneratePlutusScipts
      <$> strOption
        ( long "output-script-dir"
            <> short 'o'
            <> metavar "DIR"
        )
  genPursModule :: Parser ClArgs
  genPursModule =
    GeneratePursModule
      <$> strOption
        ( long "purs-module-name"
            <> short 'n'
            <> metavar "MODULE"
            <> help "Module name example: Fida.RawScripts"
        )
      <*> optional
        ( strOption
            ( long "output-purs-file"
                <> short 'p'
                <> metavar "PATH"
            )
        )

serialiseScriptsToPurs ::
  String ->
  [(String, Script)] ->
  IO.Handle ->
  IO ()
serialiseScriptsToPurs moduleName scripts handle = do
  let emit :: String -> IO ()
      emit = IO.hPutStrLn handle
      emit' :: C.ByteString -> IO ()
      emit' = C.hPutStrLn handle
      nl = emit ""
      scripts' = first ("raw" <>) <$> scripts

  emit "-- This file is autogenerated."
  emit $ "module " <> moduleName
  emit " ("
  case fst <$> scripts' of
    [] -> pure ()
    s : xs -> do
      emit $ "   " <> s
      for_ xs (emit . (" , " <>))
  emit " ) where"

  for_ scripts' $ \(name, script) -> do
    nl
    emit $ name <> " :: String"
    emit $ name <> " ="
    emit' $
      C.concat
        [ "   "
        , C.replicate 3 '"'
        , Aeson.encode $ serialiseToTextEnvelope Nothing $ scriptToPlutusScript script
        , C.replicate 3 '"'
        ]

  nl

serialiseScript :: FilePath -> FilePath -> Script -> IO ()
serialiseScript outputDir name script =
  let out :: PlutusScript PlutusScriptV2
      out = scriptToPlutusScript script
      file = outputDir <> "/" <> name
   in do
        putStrLn $ "Serializing " <> file
        writeFileTextEnvelope file Nothing out >>= either print pure

scriptToPlutusScript :: Script -> PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2
    . toShort
    . toStrict
    . serialise
    . Plutonomy.UPLC.optimizeUPLC
