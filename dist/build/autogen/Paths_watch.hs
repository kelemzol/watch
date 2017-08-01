{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_watch (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\ezoltke\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\ezoltke\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\watch-0.1.0.0-7aNriIIa93V1DESTG4Y0oZ"
datadir    = "C:\\Users\\ezoltke\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\watch-0.1.0.0"
libexecdir = "C:\\Users\\ezoltke\\AppData\\Roaming\\cabal\\watch-0.1.0.0-7aNriIIa93V1DESTG4Y0oZ"
sysconfdir = "C:\\Users\\ezoltke\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "watch_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "watch_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "watch_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "watch_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "watch_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
