{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_integral_philosophy (
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
bindir     = "/home/domini/src/Magazine/Magazine/haskell-project/.stack-work/install/x86_64-linux-tinfo6/3c101b6527f233e40c8f3725c84b4f4d5cbfaf7de46da6b5e312198215e328fc/9.4.8/bin"
libdir     = "/home/domini/src/Magazine/Magazine/haskell-project/.stack-work/install/x86_64-linux-tinfo6/3c101b6527f233e40c8f3725c84b4f4d5cbfaf7de46da6b5e312198215e328fc/9.4.8/lib/x86_64-linux-ghc-9.4.8/integral-philosophy-0.1.0.0-F9EYmqtsgNYB4qWnHOGDJf"
dynlibdir  = "/home/domini/src/Magazine/Magazine/haskell-project/.stack-work/install/x86_64-linux-tinfo6/3c101b6527f233e40c8f3725c84b4f4d5cbfaf7de46da6b5e312198215e328fc/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/domini/src/Magazine/Magazine/haskell-project/.stack-work/install/x86_64-linux-tinfo6/3c101b6527f233e40c8f3725c84b4f4d5cbfaf7de46da6b5e312198215e328fc/9.4.8/share/x86_64-linux-ghc-9.4.8/integral-philosophy-0.1.0.0"
libexecdir = "/home/domini/src/Magazine/Magazine/haskell-project/.stack-work/install/x86_64-linux-tinfo6/3c101b6527f233e40c8f3725c84b4f4d5cbfaf7de46da6b5e312198215e328fc/9.4.8/libexec/x86_64-linux-ghc-9.4.8/integral-philosophy-0.1.0.0"
sysconfdir = "/home/domini/src/Magazine/Magazine/haskell-project/.stack-work/install/x86_64-linux-tinfo6/3c101b6527f233e40c8f3725c84b4f4d5cbfaf7de46da6b5e312198215e328fc/9.4.8/etc"

getBinDir     = catchIO (getEnv "integral_philosophy_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "integral_philosophy_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "integral_philosophy_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "integral_philosophy_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "integral_philosophy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "integral_philosophy_sysconfdir") (\_ -> return sysconfdir)




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
