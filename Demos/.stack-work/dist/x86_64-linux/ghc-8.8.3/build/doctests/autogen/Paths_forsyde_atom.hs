{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_forsyde_atom (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
version = Version [0,3,1,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/marcello-costa/workspace/ImageProcessing/.stack-work/install/x86_64-linux/f2df9bfc7e23a5352ef8ebfda32880a27d8789c775d20370d3c5b62f3341723a/8.8.3/bin"
libdir     = "/home/marcello-costa/workspace/ImageProcessing/.stack-work/install/x86_64-linux/f2df9bfc7e23a5352ef8ebfda32880a27d8789c775d20370d3c5b62f3341723a/8.8.3/lib/x86_64-linux-ghc-8.8.3/forsyde-atom-0.3.1.1-InyllI5pWEoGfxq5azyIiz-doctests"
dynlibdir  = "/home/marcello-costa/workspace/ImageProcessing/.stack-work/install/x86_64-linux/f2df9bfc7e23a5352ef8ebfda32880a27d8789c775d20370d3c5b62f3341723a/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/marcello-costa/workspace/ImageProcessing/.stack-work/install/x86_64-linux/f2df9bfc7e23a5352ef8ebfda32880a27d8789c775d20370d3c5b62f3341723a/8.8.3/share/x86_64-linux-ghc-8.8.3/forsyde-atom-0.3.1.1"
libexecdir = "/home/marcello-costa/workspace/ImageProcessing/.stack-work/install/x86_64-linux/f2df9bfc7e23a5352ef8ebfda32880a27d8789c775d20370d3c5b62f3341723a/8.8.3/libexec/x86_64-linux-ghc-8.8.3/forsyde-atom-0.3.1.1"
sysconfdir = "/home/marcello-costa/workspace/ImageProcessing/.stack-work/install/x86_64-linux/f2df9bfc7e23a5352ef8ebfda32880a27d8789c775d20370d3c5b62f3341723a/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "forsyde_atom_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "forsyde_atom_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "forsyde_atom_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "forsyde_atom_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "forsyde_atom_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "forsyde_atom_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
