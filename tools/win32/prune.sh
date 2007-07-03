#!/bin/bash

. ./versions.conf

cd zips

## generic pruning
##################

# don't need makefiles or docs
rm -rf */make/ */man/ */share/gtk-doc/ */share/doc/

# only needed for rebuilding gtk libs
rm -rf */share/aclocal/

# don't need deverloper util progs
rm -rf *-dev-*/bin/

# don't need manifests
rm -rf */manifest/

# don't need the .a & .def files where we have .lib ones
rm -rf *-dev-*/lib/*.a *-dev-*/lib/*.def

# don't need locale files
rm -rf */lib/locale/ */share/locale/


## package-specific pruning
###########################

# don't need the gtk demo
pushd gtk+-dev-${GTK_VERSION}
rm -rf share/gtk-2.0
popd

# don't need the glib gettext stuff
pushd glib-dev-${GLIB_VERSION}
rm -rf share/glib-2.0
popd

pushd pango-${PANGO_VERSION}
rm bin/pango-querymodules.exe
popd

pushd libiconv-${LIBICONV_VERSION}.bin.woe32
rm -rf README.libiconv share/doc share/man lib/libiconv.a bin/iconv.exe
popd

pushd gettext-dev-${GETTEXT_VERSION}
rm -rf share
popd

pushd libglade-dev-${LIBGLADE_VERSION}
rm -rf share/xml
popd

pushd libxml2-dev-${LIBXML_VERSION}
rm -rf include
popd
