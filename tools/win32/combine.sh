#!/bin/bash
set -x
. ./versions.conf

pushd zips

for dir in */
do
  rm -f $dir.tar
  pushd $dir
  tar -c -f ../$(basename $dir).tar *
  popd
done

popd

rm -rf gtk2hs
mkdir -p gtk2hs/${GTK2HS_VERSION}
pushd gtk2hs/${GTK2HS_VERSION}
for tar in ../../zips/*.tar
do
  tar -xf $tar
done
chmod -x bin/*
cp -v ../../pc/*.pc lib/pkgconfig/
for file in lib/pkgconfig/*.pc ; do
  sed "s,^prefix=.*,prefix=/c/gtk2hs/${GTK2HS_VERSION}," < "$file" > "$file".new
  mv -v "$file".new "$file"
done
popd

rm -f gtk2hs-${GTK2HS_VERSION}-clibs-win32{,-dev}.zip
zip -9 -q -r gtk2hs-${GTK2HS_VERSION}-clibs-win32-dev.zip gtk2hs
echo "created gtk2hs-${GTK2HS_VERSION}-clibs-win32-dev.zip"

mkdir no-dev
cp -a gtk2hs no-dev/gtk2hs
pushd no-dev
rm -rvf \
	gtk2hs/${GTK2HS_VERSION}/bin/gdk-pixbuf-csource.exe \
	gtk2hs/${GTK2HS_VERSION}/bin/glib-* \
	gtk2hs/${GTK2HS_VERSION}/bin/gobject-query.exe \
	gtk2hs/${GTK2HS_VERSION}/bin/gtk-demo.exe \
	gtk2hs/${GTK2HS_VERSION}/bin/ior-decode-2.exe \
	gtk2hs/${GTK2HS_VERSION}/bin/orbit-idl-2.exe \
	gtk2hs/${GTK2HS_VERSION}/bin/pkg-config.exe \
	gtk2hs/${GTK2HS_VERSION}/bin/typelib-dump.exe \
	gtk2hs/${GTK2HS_VERSION}/include \
	gtk2hs/${GTK2HS_VERSION}/lib/*.lib \
	gtk2hs/${GTK2HS_VERSION}/lib/gtk-2.0/include \
	gtk2hs/${GTK2HS_VERSION}/lib/glib-2.0 \
	gtk2hs/${GTK2HS_VERSION}/lib/gtkglext-1.0 \
	gtk2hs/${GTK2HS_VERSION}/lib/pkgconfig \
	gtk2hs/${GTK2HS_VERSION}/share/idl
zip -9 -q -r ../gtk2hs-${GTK2HS_VERSION}-clibs-win32.zip gtk2hs
echo "created gtk2hs-${GTK2HS_VERSION}-clibs-win32-dev.zip"
popd

rm -rf no-dev
