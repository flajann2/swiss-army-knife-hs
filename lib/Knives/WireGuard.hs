{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Knives.WireGuard where

import Utils (systemctl)
import CommandLine
-- import System.IO
-- import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)
import Data.List

knifeWireGuard :: WireGuardOptions -> IO ()
knifeWireGuard WireGuardOptions { listWGs
                                , activateWG
                                , enableWG
                                , deactivateWG
                                , disableWG
                                , reactivateWG}
  | checkMutualExclusivity = do
      wgs <- wgList
      ss  <- wgStatus wgs
      when listWGs $ putStrLn $ formatStatus ss
      case (activateWG , enableWG , deactivateWG , disableWG , reactivateWG) of
            (Just vpn  , Nothing  , False        , False     , False) -> activate vpn
            (Nothing   , Just vpn , False        , False     , False) -> enable vpn
            (Nothing   , Nothing  , True         , False     , False) -> deactivate
            (Nothing   , Nothing  , False        , True      , False) -> disable
            (Nothing   , Nothing  , False        , False     , True ) -> reactivate
            _                        -> return ()
  | otherwise = putStrLn "Options aside from --list or -l are mutually exclusive"
  where
    wgDir = "/etc/wireguard/"

    wgList :: IO [String]
    wgList = do
      rawlist <- getDirectoryContents wgDir
      let wgconf = filter (\f -> takeExtension f == ".conf") rawlist
      let wgs    = map (\f -> dropExtension f) wgconf
      return wgs
      
    wgStatus :: [String] -> IO [(String, String, String, Bool, Bool)]
    wgStatus wgs = do
      let quick = map (\w -> vpn2wg w) wgs
      let cmd_a   = ["is-active"]  <> quick
      let cmd_e   = ["is-enabled"] <> quick
      stats_a <- systemctl cmd_a
      stats_e <- systemctl cmd_e
      return $ [(wg
                , sact
                , sena
                , sact == "active"
                , sena == "enabled") | (wg, sact, sena) <- zip3 wgs stats_a stats_e ]

    formatStatus :: [(String, String, String, Bool, Bool)] -> String
    formatStatus wss = intercalate "\n" [w <> ": " <> sa <> " " <> se |(w, sa, se, _, _) <- wss]

    checkMutualExclusivity = check [ activateWG /= Nothing
                                   , enableWG   /= Nothing
                                   , deactivateWG
                                   , disableWG
                                   , reactivateWG]

    check bs   = (length $ filter id bs) `elem` [0, 1]
    vpn2wg vpn = "wg-quick@" <> vpn
    
    activate :: String -> IO ()
    activate vpn = do
      _ <- systemctl ["start", vpn2wg vpn]
      return ()

    enable :: String -> IO ()
    enable vpn = do
      _ <- systemctl ["enable", "--now", vpn2wg vpn]
      return ()
    
    deactivate :: IO ()
    deactivate   = do
      awgs <- activeWG
      mapM_ (\w -> systemctl $ ["stop"] <> [vpn2wg w]) awgs
      return ()

    disable :: IO ()
    disable = do
      awgs <- enabledWG
      mapM_ (\w -> systemctl $ ["disable", "--now"] <> [vpn2wg w]) awgs
      return ()


    reactivate :: IO ()
    reactivate   = do
      awgs <- activeWG
      mapM_ (\w -> systemctl $ ["restart"] <>  [vpn2wg w]) awgs
      return()

    activeWG :: IO [String]
    activeWG = do
      wgl <- wgList
      wgs <- wgStatus wgl
      return [s | (s, _, _, a, _) <- wgs, a]

    enabledWG :: IO [String]
    enabledWG = do
      wgl <- wgList
      wgs <- wgStatus wgl
      return [s | (s, _, _, _, e) <- wgs, e]
