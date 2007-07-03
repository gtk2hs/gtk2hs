#!/bin/bash

. ./versions.conf

cd zips

for f in *.zip
do
	rm -rf $(basename $f .zip)
	mkdir $(basename $f .zip)
	pushd $(basename $f .zip)
	unzip -q ../$f
	popd
done
