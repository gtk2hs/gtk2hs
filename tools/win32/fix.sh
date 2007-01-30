#!/bin/bash

. ./versions.conf

cd zips

## package-specific fixing
###########################

pushd cairo-dev-${CAIRO_VERSION}
sed -i -e 's/Requires.private: libpng13/#Requires.private: libpng13/' lib/pkgconfig/cairo.pc
sed -i -e 's/Libs.private: -lz -lz -lm/Libs.private: -lz -lz -lm -lpng13/' lib/pkgconfig/cairo.pc
popd

pushd gtk+-${GTK_VERSION}
echo 'gtk-theme-name = "MS-Windows"' > etc/gtk-2.0/gtkrc
popd

pushd gtk+-dev-${GTK_VERSION}
sed -i -e 's/-user32/-luser32/' -e 's/-Wl,-luuid/-luuid/' lib/pkgconfig/*.pc
popd

pushd libglade-dev-${LIBGLADE_VERSION}
mv lib/libglade-2.0.dll.a lib/glade-2.0.lib
popd

mkdir gtkglext-${GTKGLEXT_VERSION}
mkdir gtkglext-dev-${GTKGLEXT_VERSION}
mv gtkglext-win32-${GTKGLEXT_VERSION}/bin gtkglext-${GTKGLEXT_VERSION}
mv gtkglext-win32-${GTKGLEXT_VERSION}/include gtkglext-dev-${GTKGLEXT_VERSION}
mv gtkglext-win32-${GTKGLEXT_VERSION}/lib gtkglext-dev-${GTKGLEXT_VERSION}
rm -rf gtkglext-win32-${GTKGLEXT_VERSION}

pushd libxml2-dev-${LIBXML_VERSION}
rm -r readme.txt include/
rm lib/libxml2_a.lib
mv lib/libxml2.lib lib/xml2.lib
mkdir lib/pkgconfig
cp ../../libxml-2.0.pc lib/pkgconfig/
popd

