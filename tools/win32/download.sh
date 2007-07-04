#!/bin/bash

. ./versions.conf

mkdir zips
cd zips

BASE_URL=http://ftp.gnome.org/pub/GNOME/binaries/win32

wget -c $BASE_URL/glib/2.12/glib-${GLIB_VERSION}.zip || exit
wget -c $BASE_URL/glib/2.12/glib-dev-${GLIB_VERSION}.zip || exit

wget -c $BASE_URL/pango/1.16/pango-${PANGO_VERSION}.zip || exit
wget -c $BASE_URL/pango/1.16/pango-dev-${PANGO_VERSION}.zip || exit

wget -c $BASE_URL/atk/1.18/atk-${ATK_VERSION}.zip || exit
wget -c $BASE_URL/atk/1.18/atk-dev-${ATK_VERSION}.zip || exit

wget -c $BASE_URL/dependencies/cairo-${CAIRO_VERSION}.zip || exit
wget -c $BASE_URL/dependencies/cairo-dev-${CAIRO_VERSION}.zip || exit

wget -c $BASE_URL/gtk+/2.10/gtk+-${GTK_VERSION}.zip || exit
wget -c $BASE_URL/gtk+/2.10/gtk+-dev-${GTK_VERSION}.zip || exit

wget -c $BASE_URL/libglade/2.6/libglade-${LIBGLADE_VERSION}.zip || exit
wget -c $BASE_URL/libglade/2.6/libglade-dev-${LIBGLADE_VERSION}.zip || exit

wget -c http://downloads.sourceforge.net/gtkglext/gtkglext-win32-${GTKGLEXT_VERSION}.zip

wget -c $BASE_URL/gtksourceview/1.8/gtksourceview-${SOURCEVIEW_VERSION}.zip
wget -c $BASE_URL/gtksourceview/1.8/gtksourceview-dev-${SOURCEVIEW_VERSION}.zip

wget -c $BASE_URL/dependencies/libiconv-${LIBICONV_VERSION}.bin.woe32.zip || exit
wget -c $BASE_URL/dependencies/gettext-${GETTEXT_VERSION}.zip || exit
wget -c $BASE_URL/dependencies/gettext-dev-${GETTEXT_VERSION}.zip || exit

wget -c $BASE_URL/dependencies/libpng-${LIBPNG_VERSION}.zip || exit
wget -c $BASE_URL/dependencies/libjpeg-${LIBJPEG_VERSION}.zip || exit

wget -c $BASE_URL/dependencies/libxml2-${LIBXML_VERSION}.zip || exit
wget -c $BASE_URL/dependencies/libxml2-dev-${LIBXML_VERSION}.zip || exit

wget -c $BASE_URL/dependencies/zlib-${ZLIB_VERSION}.zip || exit

wget -c $BASE_URL/dependencies/pkg-config-${PKGCONFIG_VERSION}.zip || exit

#if test ${GTK_EXTRAS}
#then
#wget -c $BASE_URL/librsvg/2.16/librsvg-${LIBRSVG_VERSION}.zip
#wget -c $BASE_URL/librsvg/2.16/librsvg-dev-${LIBRSVG_VERSION}.zip
#fi
