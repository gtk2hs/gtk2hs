#!/bin/bash

. ./versions.conf

cd zips

## package-specific fixing
###########################

pushd cairo-dev-${CAIRO_VERSION}
sed -i -e 's/Requires.private: libpng13/#Requires.private: libpng13/' lib/pkgconfig/cairo.pc
sed -i -e 's/Libs.private: -lz -lz -lm/Libs.private: -lz -lz -lm -lpng13/' lib/pkgconfig/cairo.pc
popd

pushd libglade-dev-${LIBGLADE_VERSION}
mv lib/libglade-2.0.dll.a lib/glade-2.0.lib
popd

pushd libxml2-dev-${LIBXML_VERSION}
rm -r readme.txt include/
rm lib/libxml2_a.lib
mv lib/libxml2.lib lib/xml2.lib
mkdir lib/pkgconfig
cp ../../libxml-2.0.pc lib/pkgconfig/
popd


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
pushd gtk+-dev-${GTK_VERSION}
rm -rf share/glib-2.0
popd

pushd libiconv-${LIBICONV_VERSION}.bin.woe32
rm -rf README.libiconv share/doc share/man lib/libiconv.a
popd

pushd gettext-dev-${GETTEXT_VERSION}
rm -rf share
popd

pushd libglade-dev-${LIBGLADE_VERSION}
rm -rf share/xml
popd
