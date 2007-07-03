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

rm -rf gtk+-dev-${GTK_VERSION}-win32
mkdir gtk+-dev-${GTK_VERSION}-win32
pushd gtk+-dev-${GTK_VERSION}-win32
for tar in ../zips/*.tar
do
  tar -xf $tar
done
chmod -x bin/*
popd

rm -rf gtk+-${GTK_VERSION}-win32
cp -rl gtk+-dev-${GTK_VERSION}-win32 gtk+-${GTK_VERSION}-win32
pushd gtk+-${GTK_VERSION}-win32
  rm bin/pkg-config.exe
  rm -r include
  rm lib/*.lib
  rm -r lib/pkgconfig
  rm -r lib/*/include
  rmdir lib/glib-2.0 lib/gtkglext-1.0
popd

rm -f gtk+-dev-${GTK_VERSION}-win32.zip
zip -9 -q -r gtk+-dev-${GTK_VERSION}-win32.zip gtk+-dev-${GTK_VERSION}-win32/
echo "created gtk+-dev-${GTK_VERSION}-win32.zip"

rm -f gtk+-${GTK_VERSION}-win32.zip
zip -9 -q -r gtk+-${GTK_VERSION}-win32.zip gtk+-${GTK_VERSION}-win32/
echo "created gtk+-${GTK_VERSION}-win32.zip"
