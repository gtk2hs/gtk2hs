# Gtk2Hs

[![Build Status](https://secure.travis-ci.org/gtk2hs/gtk2hs.png)](http://travis-ci.org/gtk2hs/gtk2hs)

Gtk2Hs is a GUI library for Haskell based on GTK+. GTK+ is an extensive and mature multi-platform toolkit
for creating graphical user interfaces.

## Installing GTK+

### Linux

This can be done with something like...

    sudo apt-get install libgtk2.0-dev libgtk-3-dev

### OS X

For OS X you have at least three options (none of them perfect)

 * http://www.gtk.org/download/macos.php (webkit not currently working)
 * http://www.macports.org/ (requires that you use the macports GHC too)
 * http://brew.sh/ (still uses X11)

### Windows

TODO

## Installing Gtk2Hs from GitHub

Install the latest Cabal and cabal-meta (make sure cabal-meta winds up in your PATH)

    cabal install Cabal cabal-meta

Clone the repo from github

    git clone https://github.com/gtk2hs/gtk2hs
    cd gtk2hs

Install gtk2hs-buildtools from the repo (make sure they also wind up in your PATH)

    cabal install gtk2hs-buildtools

If you want GTK+ 3 and no GTK+ 2 suport

    cabal-meta install

or if you want both GTK+ 3 and GTK+ 2 support

    ./install-both.sh
