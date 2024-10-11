{-# LANGUAGE OverloadedStrings #-}

module Knives
    ( knifeKernel
    , knifeExtIP
    , knifeSleep
    , knifeZfsClean
    ) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Control.Monad ((>=>))
import Data.Bool (bool)
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








knifeZfsClean :: ZfsCleanOptions -> IO ()
knifeZfsClean opts = do
  putStrLn $ "Not Implemented Yet mode: " ++ show (notdefinedyet opts)
