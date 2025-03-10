-- Command Line Parser 
{-# LANGUAGE OverloadedStrings #-}

module CommandLine where

import Options.Applicative

-- Define data types for our commands and options
data Command
  = ExtIP     ExtIPOptions
  | Kernel    KernelOptions
  | Sleep     SleepOptions
  | ZfsCheck  ZfsCheckOptions
  | WireGuard WireGuardOptions
  deriving Show 

data ExtIPOptions = ExtIPOptions
  { nolocation :: Bool
  , ipv6       :: Bool
  } deriving Show

data KernelOptions = KernelOptions
  { justVersion :: Bool } deriving Show

data SleepOptions = SleepOptions
  { secondsToSleep :: Maybe Int } deriving Show

data ZfsCheckOptions = ZfsCheckOptions
  { full :: Bool } deriving Show

data WireGuardOptions = WireGuardOptions
  { listWGs      :: Bool
  , activateWG   :: Maybe String
  , deactivateWG :: Bool
  , reactivateWG :: Bool
  } deriving Show

data GlobalOptions = GlobalOptions
  { verbose :: Bool } deriving Show

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch ( long "verbose"
               <> short 'v'
               <> help "Enable verbose mode" )

extipOptionsParser :: Parser ExtIPOptions
extipOptionsParser = ExtIPOptions
  <$> switch ( long "nolocation"
               <> short 'n'
               <> help "Don't display location information" )
  <*> switch ( long "ipv6"
               <> short 'i'
               <> help "Display the IPv6 address" )

kernelOptionsParser :: Parser KernelOptions
kernelOptionsParser = KernelOptions
  <$> switch ( long "justversion"
               <> short 'j'
               <> help "Just display the numerical version number, no adjoining text." )

sleepOptionsParser :: Parser SleepOptions
sleepOptionsParser = SleepOptions
  <$> optional (option auto ( long "wait"
                              <> short 'w'
                              <> metavar "SECONDS"
                              <> help "Seconds to wait before going to sleep"))

zfscheckOptionsParser :: Parser ZfsCheckOptions
zfscheckOptionsParser = ZfsCheckOptions
  <$> switch
  ( long "full"
    <> short 'f'
    <> help "Check all kernel versions" )

wgOptionsParser :: Parser WireGuardOptions
wgOptionsParser = WireGuardOptions
  <$> switch   ( long "list"
                 <> short 'l'
                 <> help "List all installed WireGuardG VPNs")
  <*> optional (option auto ( long "activate"
                              <> short 'a'
                              <> metavar "WGVPN"
                              <> help "WireGuard VPN to activate"))
  <*> switch   ( long "deactivate"
                 <> short 'd'
                 <> help "Deactivat all WireGuard VPNs")
  <*> switch   ( long "reactivate"
                 <> short 'r'
                 <> help "Reactivate active WireGuardG VPNs")


-- Combine the subcommand parsers
commandParser :: Parser Command
commandParser = subparser
  (    command "extip"    (info (ExtIP     <$> extipOptionsParser)    (progDesc "Display external IP address"))
    <> command "kernel"   (info (Kernel    <$> kernelOptionsParser)   (progDesc "Display kernel information, both installed and currently running"))
    <> command "sleep"    (info (Sleep     <$> sleepOptionsParser)    (progDesc "Put the machine to sleep"))
    <> command "zfscheck" (info (ZfsCheck  <$> zfscheckOptionsParser) (progDesc "Check zfs availibility for the current kernel"))
    <> command "wg"       (info (WireGuard <$> wgOptionsParser)       (progDesc "Manage WireGuard VPNs"))
  )

-- Combine global options with the command parser
opts :: Parser (GlobalOptions, Command)
opts = (,) <$> globalOptionsParser <*> commandParser
