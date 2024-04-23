module Main (main) where

import Cardano.Api (PlutusScriptV2, serialiseToTextEnvelope, writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Foldable (for_)
import Fida.Contract.FidaInvestorContract qualified as FidaInvestorContract
import Fida.Contract.FidaPolicyContract qualified as FidaPolicyContract
import Fida.Contract.SystemIdMintingPolicy qualified as SystemIdMintingPolicy
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
import Plutonomy.UPLC qualified
import Plutus.V2.Ledger.Api (Script)
import System.FilePath ((</>))
import System.IO (Handle)
import Prelude

data ClArgs
  = GeneratePlutusScipts
      { gpsOutDir :: !FilePath
      }
  | GeneratePursModule
      { gpmOutFile :: !FilePath,
        gpmModuleName :: !(Maybe String)
      }
  deriving (Show)

plutusScripts :: [([Char], Script)]
plutusScripts =
  [ ("FidaContractMintingPolicy", FidaPolicyContract.serialisableFidaContractMintingPolicy),
    ("FidaContractValidator", FidaPolicyContract.serialisableFidaContractValidator),
    ("InvestorContractValidator", FidaInvestorContract.serialisableInvestorContractValidator),
    ("SystemIdMintingPolicy", SystemIdMintingPolicy.serialisableSystemIdMintingPolicy)
  ]

main :: IO ()
main = getArgs >>= app

app :: ClArgs -> IO ()
app (GeneratePlutusScipts outDir) =
  for_ plutusScripts $ (\(name, script) -> serialiseScript outDir (name <> ".script") script)
app _ = return ()

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
  Handle ->
  IO ()
serialiseScriptsToPurs = undefined

serialiseScript :: FilePath -> FilePath -> Script -> IO ()
serialiseScript outputDir name script =
  let out :: PlutusScript PlutusScriptV2
      out = scriptToPlutusScript script
      file = outputDir </> name
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
