#!/bin/sh -ex

# A script to bootstrap gtk2hs

for pkg in tools glib gio cairo pango; do 
  cd $pkg;
  if cabal configure $1; then
    cabal build;
    cabal haddock;
    cabal copy;
    cabal register;
  fi;
  cd ..;
done;

cd gtk;

cabal build;
cabal haddock;
cabal copy;
cabal register;

cp -f gtk.cabal-3 gtk.cabal;
cabal build;
cabal haddock;
cabal copy;
cabal register;


