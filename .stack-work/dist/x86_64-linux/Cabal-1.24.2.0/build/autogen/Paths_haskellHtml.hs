{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskellHtml (
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

bindir     = "/home/sebashack/haskellHtml/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/bin"
libdir     = "/home/sebashack/haskellHtml/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/lib/x86_64-linux-ghc-8.0.2/haskellHtml-0.1.0.0-BECCdle20pIAvIPQ9BjiaC"
dynlibdir  = "/home/sebashack/haskellHtml/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/sebashack/haskellHtml/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/share/x86_64-linux-ghc-8.0.2/haskellHtml-0.1.0.0"
libexecdir = "/home/sebashack/haskellHtml/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/libexec"
sysconfdir = "/home/sebashack/haskellHtml/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellHtml_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellHtml_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskellHtml_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskellHtml_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellHtml_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellHtml_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
