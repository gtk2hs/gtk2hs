#!/bin/bash
set -x
. ./versions.conf

cd zips

## generic pruning
##################

# don't need makefiles or docs
rm -rvf */make/ */share/man/ */man/ */share/gtk-doc/ */share/doc/ */src

# only needed for rebuilding gtk libs
rm -rvf */share/aclocal/

# don't need deverloper util progs
rm -rvf *-dev-*/bin/

# don't need manifests
rm -rvf */manifest/

# don't need the .a & .def files where we have .lib ones
rm -rvf *-dev-*/lib/*.a *-dev-*/lib/*.def

# don't need locale files
rm -rvf */lib/locale/ */share/locale/

rm -rvf */contrib/

## package-specific pruning
###########################

pushd gtk+-bundle_${GTK_BUNDLE_VERSION}_win32
# don't need the gtk demo
rm -rvf share/gtk-2.0/demo
rm -vf bin/pango-querymodules.exe
rm -vf lib/libz.a lib/zdll.lib
rm -rvf share/glib-2.0
popd

pushd libiconv-${LIBICONV_VERSION}.bin.woe32
rm -rf README.libiconv share/doc share/man lib/libiconv.a bin/iconv.exe
popd

#pushd gettext-dev-${GETTEXT_VERSION}
#rm -rf share
#popd

pushd gtkglext-${GTKGLEXT_VERSION}-win32
rm -rf bin/gtkglext-env.{bat,sh}
popd

find -depth -type d -empty -exec rmdir -v {} \;
