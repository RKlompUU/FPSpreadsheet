module Paths_cabal_macosx (
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
version = Version [0,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rick/.cabal/bin"
libdir     = "/home/rick/.cabal/lib/x86_64-linux-ghc-7.10.1/cabal_48hsjczglSX9mRAG5No5SX"
datadir    = "/home/rick/.cabal/share/x86_64-linux-ghc-7.10.1/cabal-macosx-0.1.0"
libexecdir = "/home/rick/.cabal/libexec"
sysconfdir = "/home/rick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cabal_macosx_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cabal_macosx_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cabal_macosx_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cabal_macosx_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cabal_macosx_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
