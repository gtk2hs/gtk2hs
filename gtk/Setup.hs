-- Adjustments specific to this package,
-- all Gtk2Hs-specific boilerplate is kept in
-- gtk2hs-buildtools:Gtk2HsSetup
--
import Gtk2HsSetup ( gtk2hsUserHooks, checkGtk2hsBuildtools,
                     typeGenProgram, signalGenProgram, c2hsLocal)
import Distribution.Simple ( defaultMainWithHooks )

main = do
  checkGtk2hsBuildtools [typeGenProgram, signalGenProgram, c2hsLocal]
  defaultMainWithHooks gtk2hsUserHooks

