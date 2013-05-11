module Paths_GIST (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alexander/.cabal/bin"
libdir     = "/home/alexander/.cabal/lib/GIST-0.0.1/ghc-7.4.2"
datadir    = "/home/alexander/.cabal/share/GIST-0.0.1"
libexecdir = "/home/alexander/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "GIST_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GIST_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GIST_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GIST_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
