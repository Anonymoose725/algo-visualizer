{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_algo_visualizer (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/ethan/.cabal/bin"
libdir     = "/Users/ethan/.cabal/lib/aarch64-osx-ghc-9.6.7/algo-visualizer-0.1.0.0-inplace-algo-visualizer"
dynlibdir  = "/Users/ethan/.cabal/lib/aarch64-osx-ghc-9.6.7"
datadir    = "/Users/ethan/.cabal/share/aarch64-osx-ghc-9.6.7/algo-visualizer-0.1.0.0"
libexecdir = "/Users/ethan/.cabal/libexec/aarch64-osx-ghc-9.6.7/algo-visualizer-0.1.0.0"
sysconfdir = "/Users/ethan/.cabal/etc"

getBinDir     = catchIO (getEnv "algo_visualizer_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "algo_visualizer_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "algo_visualizer_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "algo_visualizer_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "algo_visualizer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "algo_visualizer_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
