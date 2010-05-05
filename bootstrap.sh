#!/bin/sh

# A script to bootstrap gtk2hs

for pkg in glib cairo pango gtk gconf gio glade gnomevfs gtkglext gtksourceview2 soegtk svgcairo vte webkit; do 
  cd $pkg;
  if cabal configure $1; then
    cabal build;
    cabal haddock;
    cabal install;
  fi;
  cd ..;
done;
