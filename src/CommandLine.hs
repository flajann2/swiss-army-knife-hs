-- Command Line Parser 
{-# LANGUAGE OverloadedStrings #-}

module CommandLine where

import Options.Applicative
import Data.Semigroup ((<>))

-- Define data types for our commands and options
data Command
  = ExtIP ExtIPOptions
  | Kernel KernelOptions
  | Sleep SleepOptions
  | ZfsClean ZfsCleanOptions

data ExtIPOptions = ExtIPOptions
  { nolocation :: Bool
  , ipv6 :: Bool
  }

data KernelOptions = KernelOptions
  { justVersion :: Bool }

data SleepOptions = SleepOptions
  { secondsToSleep :: Maybe Int }

data ZfsCleanOptions = ZfsCleanOptions
  { notdefinedyet :: Bool }

data GlobalOptions = GlobalOptions
  { verbose :: Bool }

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode" )

extipOptionsParser :: Parser ExtIPOptions
extipOptionsParser = ExtIPOptions
  <$> switch
  ( long "nolocation"
    <> short 'n'
    <> help "Don't display location information" )
  <*> switch
  ( long "ipv6"
    <> short 'i'
    <> help "Display the IPv6 address" )

kernelOptionsParser :: Parser KernelOptions
kernelOptionsParser = KernelOptions
  <$> switch
  ( long "justversion"
    <> short 'j'
    <> help "Just display the numerical version number, no adjoining text." )

sleepOptionsParser :: Parser SleepOptions
sleepOptionsParser = SleepOptions
  <$> optional (option auto ( long "wait"
                              <> short 'w'
                              <> metavar "SECONDS"
                              <> help "Seconds to wait before going to sleep"))

zfscleanOptionsParser :: Parser ZfsCleanOptions
zfscleanOptionsParser = ZfsCleanOptions
  <$> switch
  ( long "notdefinedyet"
    <> short '0'
    <> help "Not Implemented Yet" )


-- Combine the subcommand parsers
commandParser :: Parser Command
commandParser = subparser
  (    command "extip" (info (ExtIP <$> extipOptionsParser) (progDesc "Display external IP address"))
    <> command "kernel" (info (Kernel <$> kernelOptionsParser) (progDesc "Display kernel information, both installed and currently running"))
    <> command "sleep"  (info (Sleep <$> sleepOptionsParser) (progDesc "Put the machine to sleep"))
    <> command "zfsclean" (info (ZfsClean <$> zfscleanOptionsParser) (progDesc "Not Implemented Yet"))
  )

-- Combine global options with the command parser
opts :: Parser (GlobalOptions, Command)
opts = (,) <$> globalOptionsParser <*> commandParser

-- -- Main program logic
-- commandLine :: IO ()
-- commandLine = do
  -- (globalOpts, cmd) <- execParser $ info (opts <**> helper)
    -- ( fullDesc
      -- <> progDesc "Many useful utilities, such as getting your external IP address, installed kernel, etc."
      -- <> header "Swiss Army Knife -- Many useful functions for the hacker in all of us." )
 --  
  -- -- Use the parsed options and commands
  -- putStrLn $ "Verbose mode: " ++ show (verbose globalOpts)
 --  
  -- case cmd of
    -- ExtIP extipOpts -> do
      -- knifeExtIP extioOpts
      -- -- putStrLn $ "No location mode: " ++ show (nolocation extipOpts)
      -- -- putStrLn $ "IPv6 mode: " ++ show (ipv6 extipOpts)
    -- Kernel kernelOpts -> do
      -- putStrLn $ "Just show version mode: " ++ show (justVersion kernelOpts)
    -- Sleep sleepOpts -> do
      -- putStrLn $ "Put the machine to sleep." ++ show (secondsToSleep sleepOpts)
    -- ZfsClean  zfsOpts -> do
      -- putStrLn $ "Not Implemented Yet mode: " ++ show (notdefinedyet zfsOpts)
      
