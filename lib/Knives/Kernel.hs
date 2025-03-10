module Knives.Kernel where

import CommandLine
import Network.HTTP.Simple
import Data.Aeson
import Data.Bool (bool)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Control.Monad ((>=>))
import System.Process

      
knifeKernel :: KernelOptions -> IO ()
knifeKernel opts = do
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""
  sinstalled_lts <- readProcess "pacman" ["-Q", "linux-lts"] ""

  if (not $ justVersion opts)
    then putStrLn $ "      running: " ++ skernel
                 ++ "    installed: " ++ sinstalled 
                 ++ "installed LTS: " ++ sinstalled_lts
    else putStrLn skernel

