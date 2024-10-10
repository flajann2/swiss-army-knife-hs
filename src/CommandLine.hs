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
  = ExtIP extipOptions
  | Kernel kernelOptions
  | Sleep sleepOptions
  | ZfsClean zfsCleanOptions

data UploadOptions = UploadOptions
  { uploadFile :: FilePath
  , uploadBinary :: Bool
  }

data SearchOptions = SearchOptions
  { searchRegex :: String
  }

data GlobalOptions = GlobalOptions
  { verbose :: Bool
  }

-- Define the parser for global options
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode" )

-- Define the parser for the upload command
uploadOptionsParser :: Parser UploadOptions
uploadOptionsParser = UploadOptions
  <$> argument str (metavar "FILE" <> help "File to upload")
  <*> switch
      ( long "binary"
     <> short 'b'
     <> help "Upload as binary" )

-- Define the parser for the search command
searchOptionsParser :: Parser SearchOptions
searchOptionsParser = SearchOptions
  <$> strOption
      ( long "regex"
     <> short 'r'
     <> metavar "PATTERN"
     <> help "Search pattern" )

-- Combine the subcommand parsers
commandParser :: Parser Command
commandParser = subparser
  ( command "upload" (info (Upload <$> uploadOptionsParser) (progDesc "Upload a file"))
 <> command "search" (info (Search <$> searchOptionsParser) (progDesc "Search for a pattern"))
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
    Upload uploadOpts -> do
      putStrLn $ "Uploading file: " ++ uploadFile uploadOpts
      putStrLn $ "Binary mode: " ++ show (uploadBinary uploadOpts)
    Search searchOpts -> do
      putStrLn $ "Searching with regex: " ++ searchRegex searchOpts

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
