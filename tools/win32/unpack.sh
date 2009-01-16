#!/bin/bash
set -x
. ./versions.conf

if test -z "$UNZIPPER" ; then
  UNZIPPER=unzip
fi

cd zips

for f in *.zip
do
	rm -rf $(basename $f .zip)
	mkdir $(basename $f .zip)
	pushd $(basename $f .zip)
	$UNZIPPER ../$f
	popd
done
