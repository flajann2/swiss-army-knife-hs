{-# LANGUAGE OverloadedStrings #-}

module Knives
    ( knifeKernel
    , knifeExtIP
    , knifeSleep
    , knifeZfsCheck
    , kinfeYamlMacros
    ) where

import Network.HTTP.Simple
import Data.Aeson
import Data.Bool (bool)
import Data.List (delete)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Control.Monad ((>=>))
import System.Process
import CommandLine

data RExtIP = RExtIP
  { ip :: String } deriving Show

instance FromJSON RExtIP where
  parseJSON = withObject "RExtIP" $ \v -> RExtIP
    <$> v .: "ip"

data RLoc = RLoc
  {status      :: String
  , country    :: String
  , countryCode:: String
  , region     :: String
  , regionName :: String
  , city       :: String
  , zip        :: String
  , lat        :: Float
  , lon        :: Float
  , timezone   :: String
  , isp        :: String
  , org        :: String
  , as         :: String
  , query      :: String
  } deriving Show

instance FromJSON RLoc where
  parseJSON = withObject "RLoc" $ \v -> RLoc
    <$> v .: "status"
    <*> v .: "country"
    <*> v .: "countryCode"
    <*> v .: "region"
    <*> v .: "regionName"
    <*> v .: "city"
    <*> v .: "zip"
    <*> v .: "lat"
    <*> v .: "lon"
    <*> v .: "timezone"
    <*> v .: "isp"
    <*> v .: "org"
    <*> v .: "as"
    <*> v .: "query"
  
knifeExtIP :: ExtIPOptions -> IO ()
knifeExtIP opts = do
  res <- (parseRequest >=> httpLBS) $ bool url_ipv4 url_ipv6 $ ipv6 opts
  let status_code = getResponseStatusCode res
  putStrLn $ ipaddr res
  if (not $ nolocation opts)
    then do
      loc <- location
      putStrLn $ loc
    else return ()

  where
    url_ipv4 = "https://ipv4.jsonip.com"
    url_ipv6 = "https://ipv6.jsonip.com"
    url_geo  = "http://ip-api.com/json/"
    ipaddr res = do
      let ipresult = eitherDecode $ getResponseBody res :: Either String RExtIP
      case ipresult of
        Left err -> do
          "Failed to obtain IP: " ++ err
        Right extIP -> do
          ip extIP

    location :: IO String
    location = do
      request <- parseRequest url_geo
      res <- httpLBS request
      let locres = eitherDecode $ getResponseBody res :: Either String RLoc
      case locres of
        Left err -> do
          return $ "Failed to obtain location: " ++ err
        Right loc -> do
          let str_result = "Location: "
                ++ city loc ++ ", "
                ++ country loc
                ++ " (" ++ countryCode loc ++ ")\n"
                ++ "Timezone: " ++ timezone loc ++ "\n"
                
          return str_result
      
knifeKernel :: KernelOptions -> IO ()
knifeKernel opts = do
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""
  sinstalled_lts <- readProcess "pacman" ["-Q", "linux-lts"] ""

  if (not $ justVersion opts)
    then putStrLn $ "      running: " ++ skernel
                 ++ "    installed: " ++ sinstalled 
                 ++ "installed LTS: " ++ sinstalled_lts
    else putStrLn skernel

knifeSleep :: SleepOptions -> IO ()
knifeSleep opts = do
  putStrLn $ "Put the machine to sleep." ++ show (secondsToSleep opts)
  case (secondsToSleep opts) of
    Just secs -> do putStrLn $ "sleep in " ++ show secs ++ " seconds."
                    _n <- readProcess "sleep" [show secs] ""
                    return ()
    Nothing  ->  putStrLn "sleep immediatly"
  _nn <-  readProcess "systemctl" ["suspend", "-i"] ""
  return ()

knifeZfsCheck :: ZfsCheckOptions -> IO ()
knifeZfsCheck opts = do
  putStrLn $ "Check the version of archzfs with repo kernel version so you can determine if an upgrade is possible."

  let url_zfs = "https://raw.githubusercontent.com/openzfs/zfs/master/META"

  repo_version  <- readProcess "pacman" ["-Si", "linux"] "" -- | awk '/^Version/ { print $3 }'" ""
  archzfs_version <- readProcess "curl" ["-sSL", url_zfs] ""
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""

  let repo    = splitAndTrim
                $ LBS8.unpack
                $ head [v | v <- LBS8.lines (LBS8.pack repo_version), (LBS8.pack "Version") `LBS.isPrefixOf` v]
      archzfs = splitAndTrim
                $ LBS8.unpack
                $ head [v | v <- LBS8.lines (LBS8.pack archzfs_version), (LBS8.pack "Linux-Maximum:") `LBS.isPrefixOf` v]
      running = trim skernel
      installed = unwords
                $ filter (not . ("linux" `isInfixOf`))
                $ words
                $ trim sinstalled

  putStrLn $ "\n      repo: " ++ repo
    ++ "\n       zfs: " ++ archzfs
    ++ "\n   running: " ++ running
    ++ "\n installed: " ++ installed

  where
    splitAndTrim line = trim $ last $ splitOn ":" line
    trim = f . f
      where f = reverse . dropWhile isSpace

kinfeYamlMacros :: YamlMacosOptions -> IO ()
knifeYamlMacros opts = do
  return ()
