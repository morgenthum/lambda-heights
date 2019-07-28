{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_lambda_heights_core (
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
version = Version [1,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mario/Development/intellij-workspace/lambda-heights/.stack-work/install/x86_64-osx/d3e7af164d733ff8884728e5e4645879ab1ea6706d42eeca81ff0651336051ff/8.6.5/bin"
libdir     = "/Users/mario/Development/intellij-workspace/lambda-heights/.stack-work/install/x86_64-osx/d3e7af164d733ff8884728e5e4645879ab1ea6706d42eeca81ff0651336051ff/8.6.5/lib/x86_64-osx-ghc-8.6.5/lambda-heights-core-1.2.0-1jumq4i0APS7t14Fk8ZCMc"
dynlibdir  = "/Users/mario/Development/intellij-workspace/lambda-heights/.stack-work/install/x86_64-osx/d3e7af164d733ff8884728e5e4645879ab1ea6706d42eeca81ff0651336051ff/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/mario/Development/intellij-workspace/lambda-heights/.stack-work/install/x86_64-osx/d3e7af164d733ff8884728e5e4645879ab1ea6706d42eeca81ff0651336051ff/8.6.5/share/x86_64-osx-ghc-8.6.5/lambda-heights-core-1.2.0"
libexecdir = "/Users/mario/Development/intellij-workspace/lambda-heights/.stack-work/install/x86_64-osx/d3e7af164d733ff8884728e5e4645879ab1ea6706d42eeca81ff0651336051ff/8.6.5/libexec/x86_64-osx-ghc-8.6.5/lambda-heights-core-1.2.0"
sysconfdir = "/Users/mario/Development/intellij-workspace/lambda-heights/.stack-work/install/x86_64-osx/d3e7af164d733ff8884728e5e4645879ab1ea6706d42eeca81ff0651336051ff/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lambda_heights_core_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambda_heights_core_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lambda_heights_core_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lambda_heights_core_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambda_heights_core_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambda_heights_core_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
