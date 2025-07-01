module Knives.NetMan where

import Utils
import CommandLine
import Control.Monad (when)

knifeNetMan :: NetManOptions -> IO ()
knifeNetMan NetManOptions { activateNM
                          , enableNM
                          , deactivateNM
                          , disableNM
                          , reactivateNM}
  | isExclusiveOr [ activateNM
                  , enableNM
                  , deactivateNM
                  , disableNM
                  , reactivateNM] = do
      when activateNM   activate
      when enableNM     enable
      when deactivateNM deactivate
      when disableNM    disable
      when reactivateNM reactivate
      return ()
  | otherwise = putStrLn("You must specify one and only one option for NetMan.")
  where
    service    = "NetworkManager.service"
    activate   = systemctl_ ["start",   service]
    enable     = systemctl_ ["enable", "--now", service]
    deactivate = systemctl_ ["stop",    service]
    disable    = systemctl_ ["disable", "--now", service]
    reactivate = systemctl_ ["restart", service]
