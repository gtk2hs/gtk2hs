#!/bin/bash -ex

# Install both gtk and gtk3
$CABAL install ./tools --reinstall --force-reinstall;
$CABAL install ./glib --reinstall --force-reinstall;
$CABAL install ./gio --reinstall --force-reinstall;
$CABAL install ./cairo --reinstall --force-reinstall;
$CABAL install ./pango --reinstall --force-reinstall;

mv gtk/gtk.cabal-renamed gtk/gtk.cabal || true
mv gtk/gtk3.cabal gtk/gtk3.cabal-renamed || true
$CABAL install ./gtk --reinstall --force-reinstall;

rm -rf gtk/dist

mv gtk/gtk3.cabal-renamed gtk/gtk3.cabal || true
mv gtk/gtk.cabal gtk/gtk.cabal-renamed || true
$CABAL install ./gtk -fbuild-demos --reinstall --force-reinstall;

