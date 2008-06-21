#!/bin/bash

. ./versions.conf

pushd zips
rm -rf */
rm -f *.tar
popd

rm -rf gtk2hs-clibs-${GTK_VERSION}
rm -f gtk2hs-clibs-${GTK_VERSION}.tar.gz

rm -rf gtk2hs-clibs-dev-${GTK_VERSION}
rm -f gtk2hs-clibs-dev-${GTK_VERSION}.tar.gz
