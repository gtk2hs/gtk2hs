#!/bin/bash

. ./versions.conf

cd zips

## package-specific fixing
###########################

pushd cairo-dev-${CAIRO_VERSION}
sed -i -e 's/Requires.private: libpng13/#Requires.private: libpng13/' lib/pkgconfig/cairo.pc
sed -i -e 's/Libs.private: -lz -lm/Libs.private: -lm -lpng13/' lib/pkgconfig/cairo.pc
sed -i -e 's/-lz//' lib/pkgconfig/cairo-pdf.pc
popd

pushd gtk+-${GTK_VERSION}
echo 'gtk-theme-name = "MS-Windows"' > etc/gtk-2.0/gtkrc
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
rm lib/libxml2_a.lib
mv lib/libxml2.lib lib/xml2.lib
sed -i -e 's/-lz//' lib/pkgconfig/libxml-2.0.pc
popd

if test ${GTK_EXTRAS}
then
pushd librsvg-dev-${LIBRSVG_VERSION}
mv lib/librsvg-2.dll.a lib/rsvg-2.lib
popd

pushd gtksourceview-dev-${SOURCEVIEW_VERSION}
mv lib/libgtksourceview-1.0.dll.a lib/gtksourceview-1.0.lib
sed -i -e 's/libgnomeprint-2.2//' lib/pkgconfig/gtksourceview-1.0.pc 
popd
fi
