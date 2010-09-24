-- Setup file for a Gtk2Hs module. Contains only adjustments specific to this module,
-- all Gtk2Hs-specific boilerplate is stored in Gtk2HsSetup.hs which should be kept
-- identical across all modules.
import Gtk2HsSetup ( gtk2hsUserHooks, checkGtk2hsBuildtools )
import Distribution.Simple ( defaultMainWithHooks )

main = do
  checkGtk2hsBuildtools ["gtk2hsC2hs", "gtk2hsTypeGen", "gtk2hsHookGenerator"]
  defaultMainWithHooks gtk2hsUserHooks
  
