#!/bin/sh

# A script to bootstrap gtk2hs

for pkg in glib cairo pango gtk gconf gio glade gnomevfs gtkglext gtksourceview2 soegtk svgcairo vte webkit; do cd $pkg; cabal clean; cabal configure --user; cabal build; cabal haddock; cabal install; cd ..; done;
