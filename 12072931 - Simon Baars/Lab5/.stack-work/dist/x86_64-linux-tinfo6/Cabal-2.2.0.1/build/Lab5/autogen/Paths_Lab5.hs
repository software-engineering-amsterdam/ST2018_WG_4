{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Lab5 (
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

bindir     = "/home/simon/haskell/ST2018_WG_4/12072931 - Simon Baars/Lab5/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/bin"
libdir     = "/home/simon/haskell/ST2018_WG_4/12072931 - Simon Baars/Lab5/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3/Lab5-0.1.0.0-L9i6O0GLcy76KZbPwboxn0-Lab5"
dynlibdir  = "/home/simon/haskell/ST2018_WG_4/12072931 - Simon Baars/Lab5/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/simon/haskell/ST2018_WG_4/12072931 - Simon Baars/Lab5/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/share/x86_64-linux-ghc-8.4.3/Lab5-0.1.0.0"
libexecdir = "/home/simon/haskell/ST2018_WG_4/12072931 - Simon Baars/Lab5/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/libexec/x86_64-linux-ghc-8.4.3/Lab5-0.1.0.0"
sysconfdir = "/home/simon/haskell/ST2018_WG_4/12072931 - Simon Baars/Lab5/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Lab5_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Lab5_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Lab5_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Lab5_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab5_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab5_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
