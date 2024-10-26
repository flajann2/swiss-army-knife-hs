{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Knives.YamlMacros where


import GHC.Generics
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Data.Yaml.Combinators
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

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
import Options.Applicative.Types (ParserM, OptVisibility)

data Flavor a = Switch Bool
              | Parm a
  deriving (Show, Generic)

data ParmType = PTInt Integer
              | PTFloat Float
              | PTString String
  deriving (Show, Generic)

data Option = Option
  { optype :: OptType
  , long :: String
  , short :: Char
  , meta :: String
  , ptype :: ParmType
  , help :: String
  } deriving (Show, Generic)

data Knife = Knife
  { command :: String    -- subcommand name, lowercased from the Yaml
  , option  :: Vector Option
  , action  :: Action 
  } deriving (Show, Generic)

data Macros = Macros
  { name :: String
  , description :: String
  , author :: String
  , copyright :: String
  , knives :: Vector Knife
  } deriving (Show, Generic)


knifeYamlMacros :: YamlMacrosOptions -> IO ()
knifeYamlMacros opts = do
  return ()
