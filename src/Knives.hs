{-# LANGUAGE OverloadedStrings #-}

module Knives
    ( knifeKernel
    , knifeExtIP
    , knifeSleep
    , knifeZfsClean
    ) where

import CommandLine


knifeExtIP :: ExtIPOptions -> IO ()
knifeExtIP opts = do
  putStrLn $ "No location mode: " ++ show (nolocation opts)
  putStrLn $ "IPv6 mode: " ++ show (ipv6 opts)
  
knifeKernel :: KernelOptions -> IO ()
knifeKernel opts = do
  putStrLn $ "Just show version mode: " ++ show (justVersion opts)

knifeSleep :: SleepOptions -> IO ()
knifeSleep opts = do
  putStrLn $ "Put the machine to sleep." ++ show (secondsToSleep opts)

knifeZfsClean :: ZfsCleanOptions -> IO ()
knifeZfsClean opts = do
  putStrLn $ "Not Implemented Yet mode: " ++ show (notdefinedyet opts)










