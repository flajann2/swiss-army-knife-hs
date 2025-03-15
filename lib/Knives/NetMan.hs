module Knives.NetMan where

import Utils
import CommandLine
import Control.Monad (when)

knifeNetMan :: NetManOptions -> IO ()
knifeNetMan NetManOptions { activateNM
                          , deactivateNM
                          , reactivateNM}
  | isExclusiveOr [ activateNM
                  , deactivateNM
                  , reactivateNM] = do
      _ <- when activateNM   activate
      _ <- when deactivateNM deactivate
      _ <- when reactivateNM reactivate
      return ()
  | otherwise = putStrLn("You must specify one and only one option for NetMan.")
  where
    service = "NetworkManager.service"
    activate   = systemctl_ ["start",   service]
    deactivate = systemctl_ ["stop",    service]
    reactivate = systemctl_ ["restart", service]
