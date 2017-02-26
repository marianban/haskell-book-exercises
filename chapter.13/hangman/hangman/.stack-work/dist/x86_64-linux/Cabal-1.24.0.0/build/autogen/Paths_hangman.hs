{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hangman (
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

bindir     = "/home/majo/Desktop/haskell/chapter.13/hangman/hangman/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/bin"
libdir     = "/home/majo/Desktop/haskell/chapter.13/hangman/hangman/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/lib/x86_64-linux-ghc-8.0.1/hangman-0.1.0.0-4toxJBVGt7pHY1oKp1zCd2"
datadir    = "/home/majo/Desktop/haskell/chapter.13/hangman/hangman/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/share/x86_64-linux-ghc-8.0.1/hangman-0.1.0.0"
libexecdir = "/home/majo/Desktop/haskell/chapter.13/hangman/hangman/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/libexec"
sysconfdir = "/home/majo/Desktop/haskell/chapter.13/hangman/hangman/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hangman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hangman_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hangman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hangman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hangman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
