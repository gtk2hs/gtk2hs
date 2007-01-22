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

rm -rf libxml2-dev-${LIBXML_VERSION}/
mv libxml2-${LIBXML_VERSION}.win32 libxml2-dev-${LIBXML_VERSION}
mv libxml2-dev-${LIBXML_VERSION}/libxml2-${LIBXML_VERSION}.win32/* libxml2-dev-${LIBXML_VERSION}/
rmdir libxml2-dev-${LIBXML_VERSION}/libxml2-${LIBXML_VERSION}.win32/
