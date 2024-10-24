module Knives.Sleep where

import Network.HTTP.Simple
import Data.Aeson
import Data.Bool (bool)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Control.Monad ((>=>), unless)
import System.Process
import CommandLine
      
knifeSleep :: SleepOptions -> IO ()
knifeSleep opts = do
  putStrLn $ "Put the machine to sleep." ++ show (secondsToSleep opts)
  case secondsToSleep opts of
    Just secs -> do putStrLn $ "sleep in " ++ show secs ++ " seconds."
                    _n <- readProcess "sleep" [show secs] ""
                    return ()
    Nothing  ->  putStrLn "sleep immediatly"
  _nn <-  readProcess "systemctl" ["suspend", "-i"] ""
  return ()
