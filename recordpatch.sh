#!/bin/sh

# A script to record patch faster for gtk2hs developers.

for pkg in tools glib gio cairo pango gtk; do 
  cd $pkg;
  darcs record;
  cd ..;
done;
