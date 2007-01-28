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

rm -rf gtk+-dev-${GTK_VERSION}
mkdir gtk+-dev-${GTK_VERSION}
pushd gtk+-dev-${GTK_VERSION}
for tar in ../zips/*.tar
do
  tar -xf $tar
done
chmod -x bin/*
popd

cp -rl gtk+-dev-${GTK_VERSION} gtk+-${GTK_VERSION}
pushd gtk+-${GTK_VERSION}
  rm -r include lib
  rm bin/pkg-config.exe
popd

rm -f gtk+-dev-${GTK_VERSION}.tar.gz
tar -c gtk+-dev-${GTK_VERSION} -zf gtk+-dev-${GTK_VERSION}.tar.gz
echo "created gtk+-dev-${GTK_VERSION}.tar.gz"

rm -f gtk+-${GTK_VERSION}.tar.gz
tar -c gtk+-${GTK_VERSION} -zf gtk+-${GTK_VERSION}.tar.gz
echo "created gtk+-${GTK_VERSION}.tar.gz"
