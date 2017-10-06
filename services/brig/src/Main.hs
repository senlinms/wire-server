module Main (main) where

import Brig.API
import OpenSSL (withOpenSSL)

import Brig.Options
import Data.Monoid
import Data.Yaml (decodeFileEither, ParseException)
import Options.Applicative
import System.Directory
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
parseConfigPath = execParser (info (helper <*> pathParser) desc)
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
