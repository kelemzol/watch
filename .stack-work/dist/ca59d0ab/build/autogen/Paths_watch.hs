{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_watch (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\ezoltke\\haskell\\watch\\.stack-work\\install\\e77882c1\\bin"
libdir     = "C:\\Users\\ezoltke\\haskell\\watch\\.stack-work\\install\\e77882c1\\lib\\x86_64-windows-ghc-8.0.2\\watch-0.1.0.0"
dynlibdir  = "C:\\Users\\ezoltke\\haskell\\watch\\.stack-work\\install\\e77882c1\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\ezoltke\\haskell\\watch\\.stack-work\\install\\e77882c1\\share\\x86_64-windows-ghc-8.0.2\\watch-0.1.0.0"
libexecdir = "C:\\Users\\ezoltke\\haskell\\watch\\.stack-work\\install\\e77882c1\\libexec"
sysconfdir = "C:\\Users\\ezoltke\\haskell\\watch\\.stack-work\\install\\e77882c1\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "watch_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "watch_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "watch_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "watch_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "watch_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "watch_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
