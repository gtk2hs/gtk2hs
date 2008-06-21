#!/bin/bash

. ./versions.conf

cd zips

## generic pruning
##################

# don't need makefiles or docs
rm -rf */make/ */share/man/ */man/ */share/gtk-doc/ */share/doc/

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

pushd gtk+-bundle-${GTK_VERSION}
# don't need the gtk demo
rm -rf share/gtk-2.0/
rmdir share/gtk-2.0
# don't need the glib gettext stuff
rm -rf share/glib-2.0/
rmdir share/glib-2.0
rm bin/pango-querymodules.exe
rm lib/libz.a lib/zdll.lib
popd

pushd libiconv-${LIBICONV_VERSION}.bin.woe32
rm -rf README.libiconv share/doc share/man lib/libiconv.a bin/iconv.exe
popd

#pushd gettext-dev-${GETTEXT_VERSION}
#rm -rf share
#popd

pushd gtkglext-${GTKGLEXT_VERSION}
rm -rf bin/gtkglext-env.{bat,sh}
popd

find -depth -type d -empty -exec rmdir -v {} \;
