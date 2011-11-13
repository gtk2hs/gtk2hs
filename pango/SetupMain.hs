-- The real Setup file for a Gtk2Hs package (invoked via the SetupWrapper).
-- It contains only adjustments specific to this package,
-- all Gtk2Hs-specific boilerplate is kept in Gtk2HsSetup.hs
-- which should be kept identical across all packages.
--
import Distribution.Simple ( defaultMainWithHooks, UserHooks(postConf),
                             PackageIdentifier(..), PackageName(..) )
import Gtk2HsSetup ( gtk2hsUserHooks, getPkgConfigPackages, checkGtk2hsBuildtools,
                     typeGenProgram, signalGenProgram, c2hsLocal)
import Distribution.Simple.Setup ( ConfigFlags(configVerbosity), fromFlag)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Text ( display )
import Distribution.Version ( Version(..) )
import Distribution.Verbosity
import Distribution.Simple.Utils
import System.FilePath

main = do
  checkGtk2hsBuildtools [c2hsLocal, typeGenProgram]
  defaultMainWithHooks gtk2hsUserHooks {

    postConf = \args cf pd lbi -> do
      let verb = (fromFlag (configVerbosity cf))
      cPkgs <- getPkgConfigPackages verb lbi pd
      let [pangoVersion] = [ v | PackageIdentifier (PackageName "pango") v <- cPkgs ]
      writePangoVersionHeaderFile verb lbi pangoVersion
      postConf gtk2hsUserHooks args cf pd lbi
  }

------------------------------------------------------------------------------
-- Generate CPP defines for version of C libs.
------------------------------------------------------------------------------

writePangoVersionHeaderFile :: Verbosity -> LocalBuildInfo -> Version -> IO ()
writePangoVersionHeaderFile verbosity lbi (Version (major:minor:micro:_) []) = do
  createDirectoryIfMissingVerbose verbosity True targetDir
  rewriteFile targetFile $ unlines
    [ "#define PANGO_VERSION_MAJOR " ++ show major
    , "#define PANGO_VERSION_MINOR " ++ show minor
    , "#define PANGO_VERSION_MICRO " ++ show micro
    ]
  where
    targetDir  = autogenModulesDir lbi
    targetFile = targetDir </> "hspangoversion.h"

writeVersionHeaderFile _ _ version =
  die $ "unexpected pango version number: " ++ display version
