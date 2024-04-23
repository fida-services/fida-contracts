module Main (main) where

import Prelude
import Options.Applicative
    ( (<**>),
      Alternative((<|>)),
      optional,
      fullDesc,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strOption,
      execParser,
      helper,
      Parser )

data ClArgs
  = GeneratePlutusScipts
      { gpsOutDir :: FilePath
      }
  | GeneratePursModule
      { gpmOutFile :: FilePath
      , gpmModuleName :: Maybe String
      }
  deriving Show

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show args

getArgs :: IO ClArgs
getArgs = execParser opts
  where
    opts = info (genPlutusScript <|> genPursModule <**> helper)
      ( fullDesc <> progDesc "Outputs plutus scripts")
    genPlutusScript :: Parser ClArgs
    genPlutusScript = GeneratePlutusScipts
                      <$> strOption
                          ( long "output-script-dir"
                         <> short 'o'
                         <> metavar "DIR" )
    genPursModule :: Parser ClArgs
    genPursModule = GeneratePursModule
                    <$> strOption
                        ( long "purs-module-name"
                        <> short 'n'
                        <> metavar "MODULE"
                        <> help "Module name example: Fida.RawScripts" )
                    <*> optional (strOption
                        ( long "output-purs-file"
                        <> short 'p'
                        <> metavar "PATH"))
