{-# LANGUAGE OverloadedStrings #-}

module Knives
    ( doTheCut
    ) where

import CommandLine

doTheCut :: IO ()
doTheCut = do
  putStrLn "doTheCut"
