#!/bin/bash

. ./versions.conf

pushd zips

for dir in */
do
  pushd $dir
  tar -c -f ../$(basename $dir).tar *
  popd
done

popd

rm -rf gtk+-${GTK_VERSION}
mkdir gtk+-${GTK_VERSION}
pushd gtk+-${GTK_VERSION}
for tar in ../zips/*.tar
do
  tar -xf $tar
done
chmod -x bin/*
popd

tar -c gtk+-${GTK_VERSION} -zf gtk+-${GTK_VERSION}.tar.gz
echo "created gtk+-${GTK_VERSION}.tar.gz"
