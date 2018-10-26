{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exam (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/simon/haskell/exam/.stack-work/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/bin"
libdir     = "/home/simon/haskell/exam/.stack-work/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/lib/x86_64-linux-ghc-8.4.3/exam-1.0-AKwYJmnrQc5GJt2ALOvmbN-exam"
dynlibdir  = "/home/simon/haskell/exam/.stack-work/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/simon/haskell/exam/.stack-work/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/share/x86_64-linux-ghc-8.4.3/exam-1.0"
libexecdir = "/home/simon/haskell/exam/.stack-work/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/libexec/x86_64-linux-ghc-8.4.3/exam-1.0"
sysconfdir = "/home/simon/haskell/exam/.stack-work/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exam_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exam_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exam_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exam_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exam_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exam_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
