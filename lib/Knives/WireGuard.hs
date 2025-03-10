
module Knives.WireGuard where

import CommandLine
import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)
import Data.List

knifeWireGuard :: WireGuardOptions -> IO ()
knifeWireGuard WireGuardOptions { listWGs
                                , activateWG
                                , deactivateWG
                                , reactivateWG}
  | checkMutualExclusivity = do
      wgs <- wgList
      ss  <- wgStatus wgs
      when listWGs $ putStrLn $ formatStatus ss
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

    wgStatus :: [String] -> IO [(String, String, Bool)]
    wgStatus wgs = do
      let quick = map (\w -> "wg-quick@" ++ w) wgs
      let cmd = ["is-active"] ++ quick
      (_, Just hout, _, _) <- createProcess (proc "systemctl" cmd) { std_out = CreatePipe }
      out <- hGetContents hout
      let stats = lines out
      print out
      return $ [(wg, sact, sact == "active") | (wg, sact) <- zip wgs stats  ]

    formatStatus :: [(String, String, Bool)] -> String
    formatStatus wss = intercalate "\n" [w ++ ": " ++ s |(w, s, _) <- wss]

-- systemctl is-active wg-quick@nyc71 wg-quick@erf91
-- import System.Process
-- 
-- main :: IO ()
-- main = do
--     (_, Just hout, _, _) <- createProcess (proc "ls" ["-l"]) { std_out = CreatePipe }
--     output <- hGetContents hout
--     putStrLn output
