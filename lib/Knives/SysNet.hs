{-# LANGUAGE OverloadedStrings #-}

module Knives.SysNet where

import Utils
import CommandLine
import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)
import Data.List

knifeSysNet :: SysNetOptions -> IO ()
knifeSysNet SysNetOptions { activateSN
                          , deactivateSN
                          , reactivateSN}
  | isExclusiveOr [ activateSN
                  , deactivateSN
                  , reactivateSN] = do
      when activateSN   activate
      when deactivateSN deactivate
      when reactivateSN reactivate
  | otherwise = putStrLn("You must specify one and only one option for SysNet.")
  where
    activate   = undefined
    deactivate = undefined
    reactivate = undefined
