module Utils where

import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)

isExclusiveOr :: [Bool] -> Bool
isExclusiveOr bs = (length $ filter id bs) == 1

isExclusiveOrNone :: [Bool] -> Bool
isExclusiveOrNone bs = (length $ filter id bs) `elem` exorno
  where 
   exorno = [0, 1]

systemctl :: [String] -> IO [String]
systemctl parms = systemG $ "sudo" : "systemctl" : parms  
      
systemG :: [String] -> IO [String]
systemG (cmd:parms) = do
  (_, Just hout, _, _) <- createProcess (proc cmd parms) { std_out = CreatePipe }
  out <- hGetContents hout
  return $ lines out
      
