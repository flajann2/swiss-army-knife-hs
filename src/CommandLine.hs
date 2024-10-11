-- Command Line Parser 
{-# LANGUAGE OverloadedStrings #-}
module CommandLine
  ( commandLine
  -- , Parms (..)
  ) where

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
  { secondsToSleep :: Integer }

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
  <$> argument auto (metavar "SECONDS" <> help "Seconds to wait before going to sleep")

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

-- Main program logic
commandLine :: IO ()
commandLine = do
  (globalOpts, cmd) <- execParser $ info (opts <**> helper)
    ( fullDesc
      <> progDesc "Many useful utilities, such as getting your external IP address, installed kernel, etc."
      <> header "Swiss Army Knife -- Many useful functions for the hacker in all of us." )
  
  -- Use the parsed options and commands
  putStrLn $ "Verbose mode: " ++ show (verbose globalOpts)
  
  case cmd of
    ExtIP extipOpts -> do
      putStrLn $ "No location mode: " ++ show (nolocation extipOpts)
      putStrLn $ "IPv6 mode: " ++ show (ipv6 extipOpts)
    Kernel kernelOpts -> do
      putStrLn $ "Just show version mode: " ++ show (justVersion kernelOpts)
    Sleep sleepOpts -> do
      putStrLn $ "Put the machine to sleep." ++ show (secondsToSleep sleepOpts)
    ZfsClean  zfsOpts -> do
      putStrLn $ "Not Implemented Yet mode: " ++ show (notdefinedyet zfsOpts)
      
-- module CommandLine
--   ( commandline
--   , Parms (..)
--   ) where
-- 
-- import Options.Applicative
-- import Data.Semigroup ((<>))
-- 
-- data Parms = Parms
--   { gel1           :: String
--   , gel2           :: String
--   , canvas_width   :: Int
--   , canvas_height  :: Int
--   , vert           :: Bool
--   , preview        :: Bool
--   , frames         :: Int
--   , frames_per_sec :: Double
--   , scans_per_sec  :: Double
--   , slit_percent   :: Int
--   , expand         :: Double
--   , image_format   :: String
--   , out            :: String
--   , viewer         :: String
--   , verbose        :: Bool
--   } deriving Show
--  
-- parms :: Parser Parms
-- parms = Parms
--         <$> strOption ( long "gel1"
--                         <> short '1'
--                         <> metavar "SOURCE1"
--                         <> help "First gel to scan"
--                       )
--         <*> strOption ( long "gel2"
--                         <> short '2'
--                         <> metavar "SOURCE2"
--                         <> help "Second gel to scan"
--                       )
--         <*> option auto ( long "canwidth"
--                         <> short 'W'
--                         <> metavar "WIDTH"
--                         <> help "Canvas (horizontal) Width"
--                         <> value 1280
--                         <> showDefault
--                         )
--         <*> option auto ( long "canheight"
--                         <> short 'H'
--                         <> metavar "HEIGHT"
--                         <> help "Canvas (vertical) Height"
--                         <> value 1024
--                         <> showDefault
--                         )
--         <*> switch ( long "vert"
--                      <> short 'r'
--                      <> help "Do vertical slit instead of horizontal"
--                    )
--         <*> switch ( long "preview"
--                      <> short 'w'
--                      <> help "View output image/video with viewer"
--                    )
--         <*> option auto ( long "frames"
--                           <> short 's'
--                           <> value 600
--                           <> showDefault
--                           <> metavar "NUMFRAMES"
--                           <> help "number of frames to generate"                      
--                         )
--         <*> option auto ( long "framespersec"
--                         <> short 'm'
--                         <> value 30
--                         <> metavar "FPS"
--                         <> showDefault
--                         <> help "Number of frames per second to generate"
--                         )
--         <*> option auto ( long "scanspersec"
--                         <> short 't'
--                         <> value 60
--                         <> showDefault
--                         <> metavar "SPS"
--                         <> help "Number of scans per second"
--                         )
--         <*> option auto ( long "slitpercentage"
--                         <> short 'c'
--                         <> value 60
--                         <> showDefault
--                         <> metavar "SP"
--                         <> help "Slit percentage"
--                         )
--          <*> option auto ( long "expand"
--                           <> short 'e'
--                           <> value 5.0
--                           <> showDefault
--                           <> metavar "FACTOR"
--                           <> help "Expansion factor -- 1 or greater. 1 is no expansion."                      
--                         )
--         <*> option auto ( long "format"
--                           <> short 'f'
--                           <> help "Output format (png, mp4) -- if png, output to directory"
--                           <> showDefault
--                           <> value "png"
--                           <> metavar "FILETYPE"
--                         )
--         <*> strOption ( long "output"
--                         <> short 'o'
--                         <> help "Output path prefix"
--                         <> metavar "OUTPUT"
--                         <> showDefault
--                         <> value "./ssn-"
--                       )
--         <*> strOption ( long "viewer"
--                         <> short 'V'
--                         <> help "program to view the output"
--                         <> metavar "VIEWER"
--                         <> value "/usr/bin/fim"
--                       )
--         <*> switch ( long "verbose"
--                      <> short 'v'
--                      <> help "Verbose output"
--                    )
--          
-- 
-- commandline :: IO Parms
-- commandline = cmd =<< execParser opts
--   where
--     opts = info (parms <**> helper)
--       ( fullDesc
--         <> progDesc "Many useful utilities, such as getting your external IP address, installed kernel, etc."
--         <> header "Swiss Army Knife -- Many useful functions for the hacker in all of us."
--       )
-- 
-- cmd :: Parms -> IO Parms
-- cmd p = do
--   return p
-- {-# LANGUAGE OverloadedStrings #-}
-- 
-- import Options.Applicative
-- import Data.Semigroup ((<>))
-- 
-- -- Define data types for our commands and options
-- data Command
  -- = Upload UploadOptions
  -- | Search SearchOptions
-- 
-- data UploadOptions = UploadOptions
  -- { uploadFile :: FilePath
  -- , uploadBinary :: Bool
  -- }
-- 
-- data SearchOptions = SearchOptions
  -- { searchRegex :: String
  -- }
-- 
-- data GlobalOptions = GlobalOptions
  -- { configFile :: FilePath
  -- , verbose :: Bool
  -- }
-- 
-- -- Define the parser for global options
-- globalOptionsParser :: Parser GlobalOptions
-- globalOptionsParser = GlobalOptions
  -- <$> strOption
      -- ( long "config"
     -- <> short 'c'
     -- <> metavar "FILE"
     -- <> help "Configuration file" )
  -- <*> switch
      -- ( long "verbose"
     -- <> short 'v'
     -- <> help "Enable verbose mode" )
-- 
-- -- Define the parser for the upload command
-- uploadOptionsParser :: Parser UploadOptions
-- uploadOptionsParser = UploadOptions
  -- <$> argument str (metavar "FILE" <> help "File to upload")
  -- <*> switch
      -- ( long "binary"
     -- <> short 'b'
     -- <> help "Upload as binary" )
-- 
-- -- Define the parser for the search command
-- searchOptionsParser :: Parser SearchOptions
-- searchOptionsParser = SearchOptions
  -- <$> strOption
      -- ( long "regex"
     -- <> short 'r'
     -- <> metavar "PATTERN"
     -- <> help "Search pattern" )
-- 
-- -- Combine the subcommand parsers
-- commandParser :: Parser Command
-- commandParser = subparser
  -- ( command "upload" (info (Upload <$> uploadOptionsParser) (progDesc "Upload a file"))
 -- <> command "search" (info (Search <$> searchOptionsParser) (progDesc "Search for a pattern"))
  -- )
-- 
-- -- Combine global options with the command parser
-- opts :: Parser (GlobalOptions, Command)
-- opts = (,) <$> globalOptionsParser <*> commandParser
-- 
-- -- Main program logic
-- main :: IO ()
-- main = do
  -- (globalOpts, cmd) <- execParser $ info (opts <**> helper)
    -- ( fullDesc
   -- <> progDesc "Example program with subcommands and options"
   -- <> header "myapp - a test for optparse-applicative" )
 --  
  -- -- Use the parsed options and commands
  -- putStrLn $ "Config file: " ++ configFile globalOpts
  -- putStrLn $ "Verbose mode: " ++ show (verbose globalOpts)
 --  
  -- case cmd of
    -- Upload uploadOpts -> do
      -- putStrLn $ "Uploading file: " ++ uploadFile uploadOpts
      -- putStrLn $ "Binary mode: " ++ show (uploadBinary uploadOpts)
    -- Search searchOpts -> do
      -- putStrLn $ "Searching with regex: " ++ searchRegex searchOpts
