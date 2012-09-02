module Paths_hpoker (
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

bindir     = "/home/kane/.cabal/bin"
libdir     = "/home/kane/.cabal/lib/hpoker-0.0.1/ghc-7.4.2"
datadir    = "/home/kane/.cabal/share/hpoker-0.0.1"
libexecdir = "/home/kane/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hpoker_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hpoker_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hpoker_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hpoker_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
