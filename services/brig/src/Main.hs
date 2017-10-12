module Main (main) where

import Brig.API
import OpenSSL (withOpenSSL)

import Brig.Options
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Yaml (decodeFileEither, ParseException)
import Options.Applicative
import System.Directory
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

getOptions :: IO Opts
getOptions = do
    path <- parseConfigPath
    file <- doesFileExist path
    if file then do
      configFile <- decodeConfigFile path
      case configFile of
        Left err -> fail $ show err
        Right opts -> return opts
    else do
      hPutStrLn stderr $ "Config file at " ++ path ++ " does not exist, falling back to command-line arguments. \n"
      parseOptions

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

parseConfigPath :: IO String
parseConfigPath = do
  args <- getArgs
  let result = getParseResult $ execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
      defaultPath = "/etc/wire/brig/brig.yaml"
  pure $ fromMaybe defaultPath result
  where
    pathParser :: Parser String
    pathParser = strOption $
                 long "config-file"
                 <> short 'c'
                 <> help "Config file to load"
                 <> showDefault
                 <> value "/etc/wire/brig/brig.yaml"
    desc = header "Brig - User Service" <> fullDesc

main :: IO ()
main = withOpenSSL $ do
  options <- getOptions
  runServer options
