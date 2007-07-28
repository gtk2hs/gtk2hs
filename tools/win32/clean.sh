#!/bin/bash

. ./versions.conf

rm -rf zips/*/
rm -f zips/*.tar

rm -rf gtk+-${GTK_VERSION}
rm -f gtk+-${GTK_VERSION}.tar.gz

rm -rf gtk+-dev-${GTK_VERSION}
rm -f gtk+-dev-${GTK_VERSION}.tar.gz
