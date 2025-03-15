{-# LANGUAGE OverloadedStrings #-}

module Knives.NetMan where
  
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
                          , reactivateNM} = do
  case (activateNM, deactivateNM, reactivateNM) of
    (True,  False, False) -> activate
    (False, True,  False) -> deactivate
    (False, False, True)  -> reactivate
    (_, _, _)             -> putStrLn("You must specify one and only one option.")
    where
      activate   = undefined
      deactivate = undefined
      reactivate = undefined
