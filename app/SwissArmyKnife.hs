-- main for Swiss Army Knife, cli.

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import CommandLine
import Knives

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser $ info (opts <**> helper)
    ( fullDesc
      <> progDesc "Many useful utilities, such as getting your external IP address, installed kernel, controlling your WireGuard, etc."
      <> header "Swiss Army Knife -- Many useful functions for the hacker in all of us." )
  
  case cmd of
    ExtIP extipOpts     -> knifeExtIP     extipOpts
    Kernel kernelOpts   -> knifeKernel    kernelOpts
    Sleep sleepOpts     -> knifeSleep     sleepOpts 
    Version versionOpts -> knifeVersion   versionOpts 
    ZfsCheck zfsOpts    -> knifeZfsCheck  zfsOpts
    WireGuard wgOpts    -> knifeWireGuard wgOpts
    
--- versionInfo :: Parser (a -> a)
--- versionInfo = infoOption versionString
---   (  long "version"
---   <> short 'v'
---   <> help "Show version information" )
---   where versionString = "Swiss Army Knife - Version 0.0.1.0"
