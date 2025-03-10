{-# LANGUAGE OverloadedStrings #-}

module Knives.Sleep where

import System.Process
import CommandLine

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
