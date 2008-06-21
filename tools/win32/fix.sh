#!/bin/bash

. ./versions.conf

cd zips

## package-specific fixing
###########################

#pushd cairo-dev-${CAIRO_VERSION}
#sed -i -e 's/Requires.private: libpng13/#Requires.private: libpng13/' lib/pkgconfig/cairo.pc
#sed -i -e 's/Libs.private: -lz -lm/Libs.private: -lm -lpng13/' lib/pkgconfig/cairo.pc
#sed -i -e 's/-lz//' lib/pkgconfig/cairo-pdf.pc
#popd

pushd gtk+-bundle-${GTK_VERSION}
echo 'gtk-theme-name = "MS-Windows"' > etc/gtk-2.0/gtkrc
popd

# the gtkglext binaries are now only available as an installer
# copy them from where the installer put them
mkdir gtkglext-${GTKGLEXT_VERSION}
mkdir gtkglext-dev-${GTKGLEXT_VERSION}
cp -av /c/GtkGLExt/1.0/bin gtkglext-${GTKGLEXT_VERSION}
cp -av /c/GtkGLExt/1.0/{lib,include} gtkglext-dev-${GTKGLEXT_VERSION}

rm -vf libxml2-dev-${LIBXML_VERSION}/lib/libxml2_a.lib

rm */lib/*.def
for file in */lib/lib*.dll.a ; do
  mv -v "$file" "$(echo "$file" | sed 's,/lib\([^/]*\)\.dll\.a,/\1.lib,' )"
done
for file in */lib/lib*.lib ; do
  mv -v "$file" "$(echo "$file" | sed 's,/lib\([^/]*\)\.lib,/\1.lib,' )"
done


