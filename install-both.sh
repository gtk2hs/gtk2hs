#!/bin/sh -ex

# Install both gtk and gtk3
cabal install ./tools --reinstall --force-reinstall;
cabal install ./glib --reinstall --force-reinstall;
cabal install ./gio --reinstall --force-reinstall;
cabal install ./cairo --reinstall --force-reinstall;
cabal install ./pango --reinstall --force-reinstall;

mv gtk/gtk.cabal-renamed gtk/gtk.cabal || true
mv gtk/gtk3.cabal gtk/gtk3.cabal-renamed || true
cabal install ./gtk --reinstall --force-reinstall;

rm -rf gtk/dist

mv gtk/gtk3.cabal-renamed gtk/gtk3.cabal || true
mv gtk/gtk.cabal gtk/gtk.cabal-renamed || true
cabal install ./gtk -fbuild-demos --reinstall --force-reinstall;

