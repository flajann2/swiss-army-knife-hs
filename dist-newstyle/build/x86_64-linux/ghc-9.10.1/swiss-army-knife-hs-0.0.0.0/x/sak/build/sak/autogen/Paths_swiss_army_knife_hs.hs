{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_swiss_army_knife_hs (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/eltoro/.cabal/bin"
libdir     = "/home/eltoro/.cabal/lib/x86_64-linux-ghc-9.10.1-7767/swiss-army-knife-hs-0.0.0.0-inplace-sak"
dynlibdir  = "/home/eltoro/.cabal/lib/x86_64-linux-ghc-9.10.1-7767"
datadir    = "/home/eltoro/.cabal/share/x86_64-linux-ghc-9.10.1-7767/swiss-army-knife-hs-0.0.0.0"
libexecdir = "/home/eltoro/.cabal/libexec/x86_64-linux-ghc-9.10.1-7767/swiss-army-knife-hs-0.0.0.0"
sysconfdir = "/home/eltoro/.cabal/etc"

getBinDir     = catchIO (getEnv "swiss_army_knife_hs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "swiss_army_knife_hs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "swiss_army_knife_hs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "swiss_army_knife_hs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "swiss_army_knife_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "swiss_army_knife_hs_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
