#!/bin/sh -ex

# Use cabal-src-install to install the source packages locally

# 0.X (defaults to) Gtk2
cp -f gtk/gtk.cabal-0 gtk/gtk.cabal;
cabal-src-install --src-only;

# 3.X (defaults to) Gtk3
cp -f gtk/gtk.cabal-3 gtk/gtk.cabal;
(cd gtk; cabal-src-install --src-only);

# Check the packages work
cabal install gtk2hs-buildtools --reinstall --force-reinstall;
cabal install glib --reinstall --force-reinstall;
cabal install gio --reinstall --force-reinstall;
cabal install cairo --reinstall --force-reinstall;
cabal install pango --reinstall --force-reinstall;
cabal install gtk --constraint='gtk<=1' --reinstall --force-reinstall;
cabal install gtk --constraint='gtk>=3' --reinstall --force-reinstall;

