module Paths_syz (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/dao29/.cabal/bin"
libdir     = "/home/dao29/.cabal/lib/syz-0.2.0.0/ghc-7.6.2"
datadir    = "/home/dao29/.cabal/share/syz-0.2.0.0"
libexecdir = "/home/dao29/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "syz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "syz_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "syz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "syz_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
