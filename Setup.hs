{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Exception
import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Setup
import Distribution.Simple.Utils          ( rawSystemExit
                                          , notice
                                          )
import Distribution.PackageDescription    ( PackageDescription(..)
                                          , BuildInfo(..)
                                          , emptyBuildInfo
                                          , updatePackageDescription
                                          )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..)
                                          , InstallDirs(..)
                                          , absoluteInstallDirs
                                          )
import System.Directory                   ( createDirectoryIfMissing
                                          , setCurrentDirectory
                                          , getCurrentDirectory
                                          )

import System.FilePath                    ( (</>)
                                          )


------------------------------------------------------------------------
-- Utilities

-- | Run computation with current directory.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory p m = do
  origDir <- getCurrentDirectory
  (setCurrentDirectory p >> m) `finally` setCurrentDirectory origDir

-- | Update library build infomation.
updateLibBuild :: PackageDescription -> BuildInfo -> PackageDescription
updateLibBuild pkg_desc bi = updatePackageDescription (Just bi, []) pkg_desc

-- | Get the absolute path to the build directory.  This is used during the
-- cabal build so that we can direct where to install blt to during the cabal
-- build.
getBuildPath :: LocalBuildInfo -> IO FilePath
getBuildPath lcl_build_info = do
  origDir <- getCurrentDirectory
  return $ origDir </> buildDir lcl_build_info

-- | Generate BuildInfo that would add extra library directory.
extraLibDir :: FilePath -> BuildInfo
extraLibDir path = emptyBuildInfo { extraLibDirs = [path] }

------------------------------------------------------------------------
-- Commands for compiling and building BLT.

-- | Build libblt.
buildBLT :: Verbosity -> IO ()
buildBLT verb = do
  withCurrentDirectory "libblt" $ do
    rawSystemExit verb "cp" ["config.mk.example", "config.mk"]  -- use default config
    rawSystemExit verb "make" ["print"]                         -- print build environment
    rawSystemExit verb "make" []                                -- execute the build

-- | Install library to specified directly.
installBLT :: Verbosity -> FilePath -> IO ()
installBLT verb dest = do
  notice verb ("blt/Setup.hs@installBLT: copying libblt.a library to " ++ dest)
  createDirectoryIfMissing True dest
  rawSystemExit verb "cp" [ "libblt/libblt.a", dest ]
  rawSystemExit verb "ranlib" [ dest </> "libblt.a" ]
  notice verb "blt/Setup.hs@installBLT: done."


------------------------------------------------------------------------
-- Build system hooks.

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { postConf = postConfHook
    , buildHook = doBuild
    , cleanHook = doClean
    , postInst = postInstHook
    , postCopy = postCopyHook
    }

-- | Command to run after "cabal configure" command.
postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postConfHook a f pkg_desc local_build_info = do
  let verb = fromFlag (configVerbosity f)
  buildPath <- getBuildPath local_build_info
  -- Build BLT
  buildBLT verb
  -- Install 'libblt' to build directory 'buildPath' for Haskell to link against.
  installBLT verb buildPath
  -- Modify config to use blt path.
  let p' = pkg_desc `updateLibBuild` extraLibDir buildPath
  -- Call standard postConf hook
  postConf simpleUserHooks a f p' local_build_info

-- | Buld libblt prior to building other components.
doBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
doBuild pkg_desc local_build_info h f = do
  buildPath <- getBuildPath local_build_info
  -- Add library and blt to package.
  let p' = pkg_desc `updateLibBuild` extraLibDir buildPath
  -- call standard build hook
  buildHook simpleUserHooks p' local_build_info h f

-- | Install BLT library after other installation occured.
postInstHook ::  Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postInstHook args flags pkg_desc local_build_info = do
  let verb = fromFlag (installVerbosity flags)
  -- Get install directory.
  let instDirs = absoluteInstallDirs pkg_desc local_build_info NoCopyDest
  -- Install BLT library into that path.
  installBLT verb (libdir instDirs)
  -- Call standard postInst hook
  postInst simpleUserHooks args flags pkg_desc local_build_info

-- | Install libblt library after rest of "cabal copy" command.
postCopyHook :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyHook args flags pkg_desc local_build_info = do
  let verb     = fromFlag $ copyVerbosity flags
  let instDirs = absoluteInstallDirs pkg_desc local_build_info (fromFlag (copyDest flags))
  -- Install BLT
  installBLT verb (libdir instDirs)
  -- Call standard postCopy hook
  postCopy simpleUserHooks args flags pkg_desc local_build_info

-- | Add cleanup of C++ library artifacts in addition to standard clean.
doClean :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
doClean p () u c = do
  -- cleanup c++ library artifacts
  withCurrentDirectory "libblt" $
    rawSystemExit normal "make" ["clean"]
  -- Clean up dist artifacts.
  cleanHook simpleUserHooks p () u c
