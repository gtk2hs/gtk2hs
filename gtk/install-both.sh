#!/bin/sh -ex

cabal clean
mv gtk.cabal-renamed gtk.cabal || true
mv gtk3.cabal gtk3.cabal-renamed || true
cabal-src-install "$@"

cabal clean
mv gtk3.cabal-renamed gtk3.cabal || true
mv gtk.cabal gtk.cabal-renamed || true
cabal-src-install "$@"

