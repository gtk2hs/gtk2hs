#!/bin/bash -ex

# Install both gtk and gtk3
cabal install ./tools --reinstall --force-reinstall;
cabal install ./glib --reinstall --force-reinstall --disable-documentation;
cabal install ./gio --reinstall --force-reinstall --disable-documentation;
cabal install ./cairo --reinstall --force-reinstall --disable-documentation;
cabal install ./pango --reinstall --force-reinstall --disable-documentation;

mv gtk/gtk.cabal-renamed gtk/gtk.cabal || true
mv gtk/gtk3.cabal gtk/gtk3.cabal-renamed || true
cabal install ./gtk --reinstall --force-reinstall --disable-documentation;

rm -rf gtk/dist

mv gtk/gtk3.cabal-renamed gtk/gtk3.cabal || true
mv gtk/gtk.cabal gtk/gtk.cabal-renamed || true
cabal install ./gtk -fbuild-demos --reinstall --force-reinstall --disable-documentation;

