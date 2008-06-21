#!/bin/bash

. ./versions.conf

mkdir zips
cd zips

BASE_URL=http://ftp.gnome.org/pub/GNOME/binaries/win32

#wget -c $BASE_URL/glib/2.12/glib-${GLIB_VERSION}.zip 
#wget -c $BASE_URL/glib/2.12/glib-dev-${GLIB_VERSION}.zip 

#wget -c $BASE_URL/pango/1.16/pango-${PANGO_VERSION}.zip 
#wget -c $BASE_URL/pango/1.16/pango-dev-${PANGO_VERSION}.zip 

#wget -c $BASE_URL/atk/1.18/atk-${ATK_VERSION}.zip 
#wget -c $BASE_URL/atk/1.18/atk-dev-${ATK_VERSION}.zip 

#wget -c $BASE_URL/dependencies/cairo-${CAIRO_VERSION}.zip 
#wget -c $BASE_URL/dependencies/cairo-dev-${CAIRO_VERSION}.zip 

#wget -c $BASE_URL/gtk+/2.10/gtk+-${GTK_VERSION}.zip 
#wget -c $BASE_URL/gtk+/2.10/gtk+-dev-${GTK_VERSION}.zip 

wget -c $BASE_URL/gtk+/2.12/gtk+-bundle-${GTK_VERSION}.zip 

wget -c $BASE_URL/libglade/2.6/libglade-${LIBGLADE_VERSION}.zip 
wget -c $BASE_URL/libglade/2.6/libglade-dev-${LIBGLADE_VERSION}.zip 

#wget -c http://downloads.sourceforge.net/gtkglext/gtkglext-win32-${GTKGLEXT_VERSION}.zip

#wget -c $BASE_URL/gtksourceview/1.8/gtksourceview-${SOURCEVIEW_VERSION}.zip
#wget -c $BASE_URL/gtksourceview/1.8/gtksourceview-dev-${SOURCEVIEW_VERSION}.zip

wget -c $BASE_URL/dependencies/libiconv-${LIBICONV_VERSION}.bin.woe32.zip 
#wget -c $BASE_URL/dependencies/gettext-${GETTEXT_VERSION}.zip 
#wget -c $BASE_URL/dependencies/gettext-dev-${GETTEXT_VERSION}.zip 

#wget -c $BASE_URL/dependencies/libpng-${LIBPNG_VERSION}.zip 
#wget -c $BASE_URL/dependencies/libjpeg-${LIBJPEG_VERSION}.zip 

wget -c $BASE_URL/dependencies/libxml2-${LIBXML_VERSION}.zip 
wget -c $BASE_URL/dependencies/libxml2-dev-${LIBXML_VERSION}.zip 

#wget -c $BASE_URL/dependencies/zlib-${ZLIB_VERSION}.zip 

#wget -c $BASE_URL/dependencies/pkg-config-${PKGCONFIG_VERSION}.zip 

wget -c $BASE_URL/librsvg/2.22/librsvg-${LIBRSVG_VERSION}.zip
wget -c $BASE_URL/librsvg/2.22/librsvg-dev-${LIBRSVG_VERSION}.zip

wget -c $BASE_URL/GConf/2.22/GConf-${GCONF_VERSION}.zip
wget -c $BASE_URL/GConf/2.22/GConf-dev-${GCONF_VERSION}.zip

wget -c $BASE_URL/ORBit2/2.14/ORBit2-${ORBIT_VERSION}.zip
wget -c $BASE_URL/ORBit2/2.14/ORBit2-dev-${ORBIT_VERSION}.zip

wget -c $BASE_URL/gnome-vfs/2.22/gnome-vfs-${GNOMEVFS_VERSION}.zip
wget -c $BASE_URL/gnome-vfs/2.22/gnome-vfs-dev-${GNOMEVFS_VERSION}.zip

wget -c http://gstreamer.freedesktop.org/pkg/windows/releases/gstreamer/gstreamer-${GSTREAMER_VERSION}.win32{,dev}.zip
wget -c http://gstreamer.freedesktop.org/pkg/windows/releases/gst-plugins-base/gst-plugins-base-${GSTREAMER_VERSION}.win32{,dev}.zip
