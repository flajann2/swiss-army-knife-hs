-- main for Swiss Army Knife, cli.

module Main where

import CommandLine
import Knives

main :: IO ()
main = do
  parms <- commandLine
  doTheCut
