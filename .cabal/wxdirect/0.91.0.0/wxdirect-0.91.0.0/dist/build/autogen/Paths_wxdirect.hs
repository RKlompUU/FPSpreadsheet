module Paths_wxdirect (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,91,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rick/.cabal/bin"
libdir     = "/home/rick/.cabal/lib/x86_64-linux-ghc-7.10.1/wxdir_I2LnB83InUz1JWxfgQO5gG"
datadir    = "/home/rick/.cabal/share/x86_64-linux-ghc-7.10.1/wxdirect-0.91.0.0"
libexecdir = "/home/rick/.cabal/libexec"
sysconfdir = "/home/rick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wxdirect_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wxdirect_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wxdirect_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wxdirect_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wxdirect_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
