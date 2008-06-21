#!/bin/bash

. ./versions.conf

pushd zips

for dir in */
do
  rm -f $dir.tar
  pushd $dir
  tar -c -f ../$(basename $dir).tar *
  popd
done

popd

rm -rf gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32
mkdir gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32
pushd gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32
for tar in ../zips/*.tar
do
  tar -xf $tar
done
chmod -x bin/*
cp -v ../pc/*.pc lib/pkgconfig/
for file in lib/pkgconfig/*.pc ; do
  sed "s,^prefix=.*,prefix=/gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32/${GTK2HS_VERSION}," < "$file" > "$file".new
  mv -v "$file".new "$file"
done
popd

rm -rf gtk2hs-clibs-${GTK2HS_VERSION}-win32
cp -rl gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32 gtk2hs-clibs-${GTK2HS_VERSION}-win32
pushd gtk2hs-clibs-${GTK2HS_VERSION}-win32
  rm -f bin/pkg-config.exe
  rm -r include
  rm lib/*.lib
  rm -rf lib/pkgconfig
  rm -rf lib/*/include
  rm -rf lib/glib-2.0 lib/gtkglext-1.0
popd

rm -f gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32.zip
zip -9 -q -r gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32.zip gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32/
echo "created gtk2hs-clibs-dev-${GTK2HS_VERSION}-win32.zip"

rm -f gtk2hs-clibs-${GTK2HS_VERSION}-win32.zip
zip -9 -q -r gtk2hs-clibs-${GTK2HS_VERSION}-win32.zip gtk2hs-clibs-${GTK2HS_VERSION}-win32/
echo "created gtk2hs-clibs-${GTK2HS_VERSION}-win32.zip"
