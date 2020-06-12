{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_image (
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

bindir     = "/Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221/.stack-work/install/x86_64-osx/0412420184badbba9b223d7152d464babfa6b3e92ddcda09c54ab33e50036cde/8.8.3/bin"
libdir     = "/Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221/.stack-work/install/x86_64-osx/0412420184badbba9b223d7152d464babfa6b3e92ddcda09c54ab33e50036cde/8.8.3/lib/x86_64-osx-ghc-8.8.3/image-0.1.0.0-7CGfrbjG1VP9E0BqNW2az2"
dynlibdir  = "/Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221/.stack-work/install/x86_64-osx/0412420184badbba9b223d7152d464babfa6b3e92ddcda09c54ab33e50036cde/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221/.stack-work/install/x86_64-osx/0412420184badbba9b223d7152d464babfa6b3e92ddcda09c54ab33e50036cde/8.8.3/share/x86_64-osx-ghc-8.8.3/image-0.1.0.0"
libexecdir = "/Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221/.stack-work/install/x86_64-osx/0412420184badbba9b223d7152d464babfa6b3e92ddcda09c54ab33e50036cde/8.8.3/libexec/x86_64-osx-ghc-8.8.3/image-0.1.0.0"
sysconfdir = "/Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221/.stack-work/install/x86_64-osx/0412420184badbba9b223d7152d464babfa6b3e92ddcda09c54ab33e50036cde/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "image_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "image_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "image_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "image_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "image_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "image_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
