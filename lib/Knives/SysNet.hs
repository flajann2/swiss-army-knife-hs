{-# LANGUAGE OverloadedStrings #-}

module Knives.SysNet where

import CommandLine
import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)
import Data.List
