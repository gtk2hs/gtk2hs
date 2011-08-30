-- A wrapper script for Cabal Setup.hs scripts. Allows compiling the real Setup
-- conditionally depending on the Cabal version.

module SetupWrapper (setupWrapper) where

import Distribution.Package
import Distribution.Compiler
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.Compiler
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.GHC (getInstalledPackages)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version
import Distribution.Verbosity
import Distribution.Text

import System.Environment
import System.Process
import System.Exit
import System.FilePath
import System.Directory
import qualified Control.Exception as Exception
import System.IO.Error (isDoesNotExistError)

import Data.List
import Data.Char
import Control.Monad


setupWrapper :: FilePath -> IO ()
setupWrapper setupHsFile = do
  args <- getArgs
  createDirectoryIfMissingVerbose verbosity True setupDir
  compileSetupExecutable
  invokeSetupScript args

  where
    setupDir         = "dist/setup-wrapper"
    setupVersionFile = setupDir </> "setup" <.> "version"
    setupProgFile    = setupDir </> "setup" <.> exeExtension
    setupMacroFile   = setupDir </> "wrapper-macros.h"

    useCabalVersion  = Version [1,8] []
    usePackageDB     = [GlobalPackageDB, UserPackageDB]
    verbosity        = normal

    cabalLibVersionToUse comp conf = do
      savedVersion <- savedCabalVersion
      case savedVersion of
        Just version
          -> return version
        _ -> do version <- installedCabalVersion comp conf
                writeFile setupVersionFile (show version ++ "\n")
                return version

    savedCabalVersion = do
      versionString <- readFile setupVersionFile
                         `Exception.catch` \e -> if isDoesNotExistError e
                                                   then return ""
                                                   else Exception.throwIO e
      case reads versionString of
        [(version,s)] | all isSpace s -> return (Just version)
        _                             -> return Nothing

    installedCabalVersion comp conf = do
      index <- getInstalledPackages verbosity usePackageDB conf

      let cabalDep = Dependency (PackageName "Cabal")
                                (orLaterVersion useCabalVersion)
      case PackageIndex.lookupDependency index cabalDep of
        []   -> die $ "The package requires Cabal library version "
                   ++ display useCabalVersion
                   ++ " but no suitable version is installed."
        pkgs -> return $ bestVersion (map fst pkgs)
      where
        bestVersion          = maximumBy (comparing preference)
        preference version   = (sameVersion, sameMajorVersion
                               ,stableVersion, latestVersion)
          where
            sameVersion      = version == cabalVersion
            sameMajorVersion = majorVersion version == majorVersion cabalVersion
            majorVersion     = take 2 . versionBranch
            stableVersion    = case versionBranch version of
                                 (_:x:_) -> even x
                                 _       -> False
            latestVersion    = version

    -- | If the Setup.hs is out of date wrt the executable then recompile it.
    -- Currently this is GHC only. It should really be generalised.
    --
    compileSetupExecutable = do
      setupHsNewer      <- setupHsFile      `moreRecentFile` setupProgFile
      cabalVersionNewer <- setupVersionFile `moreRecentFile` setupProgFile
      let outOfDate = setupHsNewer || cabalVersionNewer
      when outOfDate $ do
        debug verbosity "Setup script is out of date, compiling..."

        (comp, conf)    <- configCompiler (Just GHC) Nothing Nothing
                             defaultProgramConfiguration verbosity
        cabalLibVersion <- cabalLibVersionToUse comp conf
        let cabalPkgid = PackageIdentifier (PackageName "Cabal") cabalLibVersion
        debug verbosity $ "Using Cabal library version " ++ display cabalLibVersion

        writeFile setupMacroFile (generateVersionMacro cabalLibVersion)

        rawSystemProgramConf verbosity ghcProgram conf $
            ["--make", setupHsFile, "-o", setupProgFile]
         ++ ghcPackageDbOptions usePackageDB
         ++ ["-package", display cabalPkgid
            ,"-cpp", "-optP-include", "-optP" ++ setupMacroFile
            ,"-odir", setupDir, "-hidir", setupDir]
      where

        ghcPackageDbOptions dbstack = case dbstack of
          (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
          (GlobalPackageDB:dbs)               -> "-no-user-package-conf"
                                               : concatMap specific dbs
          _                                   -> ierror
          where
            specific (SpecificPackageDB db) = [ "-package-conf", db ]
            specific _ = ierror
            ierror     = error "internal error: unexpected package db stack"

        generateVersionMacro :: Version -> String
        generateVersionMacro version =
          concat
            ["/* DO NOT EDIT: This file is automatically generated by Cabal */\n\n"
            ,"#define CABAL_VERSION_CHECK(major1,major2,minor) (\\\n"
            ,"  (major1) <  ",major1," || \\\n"
            ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
            ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
            ,"\n\n"
            ]
          where
            (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)

    invokeSetupScript :: [String] -> IO ()
    invokeSetupScript args = do
      info verbosity $ unwords (setupProgFile : args)
      process <- runProcess (currentDir </> setupProgFile) args
                   Nothing Nothing
                   Nothing Nothing Nothing
      exitCode <- waitForProcess process
      unless (exitCode == ExitSuccess) $ exitWith exitCode

moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModificationTime b
            ta <- getModificationTime a
            return (ta > tb)
