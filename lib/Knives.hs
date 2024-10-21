{-# LANGUAGE OverloadedStrings #-}

module Knives
    ( knifeKernel
    , knifeExtIP
    , knifeSleep
    , knifeZfsCheck
    , knifeYamlMacros
    ) where

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
import 
      
knifeKernel :: KernelOptions -> IO ()
knifeKernel opts = do
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""
  sinstalled_lts <- readProcess "pacman" ["-Q", "linux-lts"] ""

  if not $ justVersion opts
    then putStrLn $ "      running: " ++ skernel
                 ++ "    installed: " ++ sinstalled 
                 ++ "installed LTS: " ++ sinstalled_lts
    else putStrLn skernel

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

knifeZfsCheck :: ZfsCheckOptions -> IO ()
knifeZfsCheck opts = do
  putStrLn "Check the version of archzfs with repo kernel version so you can determine if an upgrade is possible."

  let url_zfs = "https://raw.githubusercontent.com/openzfs/zfs/master/META"

  repo_version  <- readProcess "pacman" ["-Si", "linux"] "" -- | awk '/^Version/ { print $3 }'" ""
  archzfs_version <- readProcess "curl" ["-sSL", url_zfs] ""
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""

  let repo    = splitAndTrim
                $ LBS8.unpack
                $ head [v | v <- LBS8.lines (LBS8.pack repo_version), LBS8.pack "Version" `LBS.isPrefixOf` v]
      archzfs = splitAndTrim
                $ LBS8.unpack
                $ head [v | v <- LBS8.lines (LBS8.pack archzfs_version), LBS8.pack "Linux-Maximum:" `LBS.isPrefixOf` v]
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

knifeYamlMacros :: YamlMacrosOptions -> IO ()
knifeYamlMacros opts = do
  return ()
