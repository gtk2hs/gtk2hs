-- Setup file for a Gtk2Hs module. Contains only adjustments specific to this module,
-- all Gtk2Hs-specific boilerplate is stored in Gtk2HsSetup.hs which should be kept
-- identical across all modules.
import Gtk2HsSetup ( gtk2hsUserHooks )
import Distribution.Simple ( defaultMainWithHooks )

main = defaultMainWithHooks gtk2hsUserHooks
