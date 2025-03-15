{-# LANGUAGE OverloadedStrings #-}

module Knives.NetMan where

import Utils
import CommandLine
import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)
import Data.List

knifeNetMan :: NetManOptions -> IO ()
knifeNetMan NetManOptions { activateNM
                          , deactivateNM
                          , reactivateNM}
  | isExclusiveOr [ activateNM
                  , deactivateNM
                  , reactivateNM] = do
      when activateNM   activate
      when deactivateNM deactivate
      when reactivateNM reactivate
  | otherwise = putStrLn("You must specify one and only one option for NetMan.")
  where
    activate   = undefined
    deactivate = undefined
    reactivate = undefined
