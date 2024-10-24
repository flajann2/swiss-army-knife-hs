{-# LANGUAGE DeriveGeneric #-}

module Knives.YamlMacros where


import GHC.Generics
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Data.Yaml.Combinators

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

knifeYamlMacros :: YamlMacrosOptions -> IO ()
knifeYamlMacros opts = do
  return ()
