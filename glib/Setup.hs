-- Standard setup file for a Gtk2Hs module.
--
-- See also:
--  * SetupMain.hs    : the real Setup script for this package
--  * Gtk2HsSetup.hs  : Gtk2Hs-specific boilerplate
--  * SetupWrapper.hs : wrapper for compat with various ghc/cabal versions

import SetupWrapper ( setupWrapper )

main = setupWrapper "SetupMain.hs"
