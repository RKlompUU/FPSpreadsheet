module Paths_grapefruit_frp (
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
version = Version [0,1,0,5] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rick/.cabal/bin"
libdir     = "/home/rick/.cabal/lib/x86_64-linux-ghc-7.10.1/grape_13JTGZXnlHQFaUK6U5sie4"
datadir    = "/home/rick/.cabal/share/x86_64-linux-ghc-7.10.1/grapefruit-frp-0.1.0.5"
libexecdir = "/home/rick/.cabal/libexec"
sysconfdir = "/home/rick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "grapefruit_frp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "grapefruit_frp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "grapefruit_frp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "grapefruit_frp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "grapefruit_frp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
