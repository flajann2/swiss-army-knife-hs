
module Knives.WireGuard where

import CommandLine
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)

knifeWireGuard :: WireGuardOptions -> IO ()
knifeWireGuard WireGuardOptions { listWGs
                                , activateWG
                                , deactivateWG
                                , reactivateWG}
  | checkMutualExclusivity = do
      wgs <- wgList
      when listWGs $ print $ wgStatus wgs
      case (activateWG, deactivateWG, reactivateWG) of
        (Just vpn, False, False) -> activate vpn
        (Nothing, True, False)   -> deactivate
        (Nothing, False, True)   -> reactivate
        _                        -> return ()
  | otherwise = print "Options aside from --list or -l are mutually exclusive"
  where
    wgDir = "/etc/wireguard/"
    wgList :: IO [String]
    wgList = do
      rawlist <- getDirectoryContents wgDir
      let wgconf = filter (\f -> takeExtension f == ".conf") rawlist
      let wgs = map (\f -> dropExtension f) wgconf
      return wgs
    checkMutualExclusivity = check [ activateWG /= Nothing
                                   , deactivateWG
                                   , reactivateWG]
    check bs = (length $ filter id bs) `elem` [0, 1]
    activate vpn = undefined
    deactivate   = undefined
    reactivate   = undefined

    wgStatus :: [String] -> IO [(String, String)]
    wgStatus wgs = do
      let quick = map (\w -> "wg-quick" ++ w) wgs
      let cmd = ["systemctl", "is-active"] ++ quick



 -- systemctl is-active wg-quick@nyc71 wg-quick@erf91
