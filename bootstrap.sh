#!/bin/sh

# A script to bootstrap gtk2hs

for pkg in tools glib gio cairo pango gtk; do 
  cd $pkg;
  if cabal configure $1; then
    cabal build;
    cabal haddock;
    cabal install;
  fi;
  cd ..;
done;
