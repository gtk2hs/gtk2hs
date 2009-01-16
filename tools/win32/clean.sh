#!/bin/bash

. ./versions.conf

pushd zips
rm -rf */
rm -f *.tar
popd

rm -rf gtk2hs
rm -f gtk2hs-${GTK2HS_VERSION}-clibs-win32{,-dev}.zip
