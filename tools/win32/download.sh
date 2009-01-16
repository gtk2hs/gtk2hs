#!/bin/bash

. ./versions.conf

mkdir zips
cd zips

GTKGLEXT_DIR=/cygdrive/c/GtkGLExt/1.0
BASE_URL=http://ftp.gnome.org/pub/GNOME/binaries/win32

wget -c $BASE_URL/gtk+/2.14/gtk+-bundle_${GTK_BUNDLE_VERSION}_win32.zip 

wget -c $BASE_URL/libglade/2.6/libglade_${LIBGLADE_VERSION}_win32.zip 
wget -c $BASE_URL/libglade/2.6/libglade-dev_${LIBGLADE_VERSION}_win32.zip 

#wget -c http://downloads.sourceforge.net/gtkglext/gtkglext-win32-${GTKGLEXT_VERSION}.zip
#gtkglext binaries are not provided as a zip any more
if test ! -d gtkglext-${GTKGLEXT_VERSION}-win32.zip; then
  pushd $GTKGLEXT_DIR
  zip -r ~-/gtkglext-${GTKGLEXT_VERSION}-win32.zip bin/ include/ lib/ share/
  popd
fi

wget -c $BASE_URL/gtksourceview/2.4/gtksourceview-${GTKSOURCEVIEW2_VERSION}.zip
wget -c $BASE_URL/gtksourceview/2.4/gtksourceview-dev-${GTKSOURCEVIEW2_VERSION}.zip

wget -c $BASE_URL/dependencies/libiconv-${LIBICONV_VERSION}.bin.woe32.zip 
#wget -c $BASE_URL/dependencies/gettext-${GETTEXT_VERSION}.zip 
#wget -c $BASE_URL/dependencies/gettext-dev-${GETTEXT_VERSION}.zip 

#wget -c $BASE_URL/dependencies/libpng-${LIBPNG_VERSION}.zip 
#wget -c $BASE_URL/dependencies/libjpeg-${LIBJPEG_VERSION}.zip 

wget -c $BASE_URL/dependencies/libxml2-${LIBXML_VERSION}.zip 
wget -c $BASE_URL/dependencies/libxml2-dev-${LIBXML_VERSION}.zip 

#wget -c $BASE_URL/dependencies/zlib-${ZLIB_VERSION}.zip 

#wget -c $BASE_URL/dependencies/pkg-config-${PKGCONFIG_VERSION}.zip 

wget -c $BASE_URL/librsvg/2.22/librsvg_${LIBRSVG_VERSION}_win32.zip
wget -c $BASE_URL/librsvg/2.22/librsvg-dev_${LIBRSVG_VERSION}_win32.zip

wget -c $BASE_URL/GConf/2.22/GConf-${GCONF_VERSION}.zip
wget -c $BASE_URL/GConf/2.22/GConf-dev-${GCONF_VERSION}.zip

wget -c $BASE_URL/ORBit2/2.14/ORBit2_${ORBIT_VERSION}_win32.zip
wget -c $BASE_URL/ORBit2/2.14/ORBit2-dev_${ORBIT_VERSION}_win32.zip

wget -c http://gstreamer.freedesktop.org/pkg/windows/releases/gstreamer/gstreamer-${GSTREAMER_VERSION}.win32{,dev}.zip
wget -c http://gstreamer.freedesktop.org/pkg/windows/releases/gst-plugins-base/gst-plugins-base-${GSTREAMER_VERSION}.win32{,dev}.zip
