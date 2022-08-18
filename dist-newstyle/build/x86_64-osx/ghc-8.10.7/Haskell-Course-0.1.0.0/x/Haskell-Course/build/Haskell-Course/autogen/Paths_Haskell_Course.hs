{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Haskell_Course (
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
bindir     = "/Users/krushan-hasura/.cabal/bin"
libdir     = "/Users/krushan-hasura/.cabal/lib/x86_64-osx-ghc-8.10.7/Haskell-Course-0.1.0.0-inplace-Haskell-Course"
dynlibdir  = "/Users/krushan-hasura/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/krushan-hasura/.cabal/share/x86_64-osx-ghc-8.10.7/Haskell-Course-0.1.0.0"
libexecdir = "/Users/krushan-hasura/.cabal/libexec/x86_64-osx-ghc-8.10.7/Haskell-Course-0.1.0.0"
sysconfdir = "/Users/krushan-hasura/.cabal/etc"

getBinDir     = catchIO (getEnv "Haskell_Course_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Haskell_Course_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Haskell_Course_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Haskell_Course_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskell_Course_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskell_Course_sysconfdir") (\_ -> return sysconfdir)




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
