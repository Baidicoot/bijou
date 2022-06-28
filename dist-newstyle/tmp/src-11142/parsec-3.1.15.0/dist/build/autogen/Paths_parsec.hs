{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_parsec (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
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
version = Version [3,1,15,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/aidan/.cabal/store/ghc-8.10.7/parsec-3.1.15.0-1232c5c59dc9c71549b4e22c96a6babd4065300a6d0fa7eb2602fe6149d3d230/bin"
libdir     = "/home/aidan/.cabal/store/ghc-8.10.7/parsec-3.1.15.0-1232c5c59dc9c71549b4e22c96a6babd4065300a6d0fa7eb2602fe6149d3d230/lib"
dynlibdir  = "/home/aidan/.cabal/store/ghc-8.10.7/parsec-3.1.15.0-1232c5c59dc9c71549b4e22c96a6babd4065300a6d0fa7eb2602fe6149d3d230/lib"
datadir    = "/home/aidan/.cabal/store/ghc-8.10.7/parsec-3.1.15.0-1232c5c59dc9c71549b4e22c96a6babd4065300a6d0fa7eb2602fe6149d3d230/share"
libexecdir = "/home/aidan/.cabal/store/ghc-8.10.7/parsec-3.1.15.0-1232c5c59dc9c71549b4e22c96a6babd4065300a6d0fa7eb2602fe6149d3d230/libexec"
sysconfdir = "/home/aidan/.cabal/store/ghc-8.10.7/parsec-3.1.15.0-1232c5c59dc9c71549b4e22c96a6babd4065300a6d0fa7eb2602fe6149d3d230/etc"

getBinDir     = catchIO (getEnv "parsec_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "parsec_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "parsec_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "parsec_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parsec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parsec_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
