#! /bin/sh

. ./win32-build.conf

PATH=${BASE_PATH}
case $1 in
	ghc-6.2.2) PATH="${PATH}:${GHC_622_PATH}";;
	ghc-6.4.2)   PATH="${PATH}:${GHC_642_PATH}";;
	ghc-6.6)   PATH="${PATH}:${GHC_66_PATH}";;
	*) echo "GHC version parameter must be one of ghc-6.2.2, ghc-6.4.2 or ghc-6.6"; exit;;
esac
case $2 in
	gtk-2.4) GTK_BASEPATH=${GTK_24_BASEPATH};;
	gtk-2.6) GTK_BASEPATH=${GTK_26_BASEPATH};;
	gtk-2.8) GTK_BASEPATH=${GTK_28_BASEPATH}; CONFIGURE_EXTRAFLAGS="--enable-cairo"
	         export PKG_CONFIG_PATH="${GTK_BASEPATH}/lib/pkgconfig";;
	gtk-2.10) GTK_BASEPATH=${GTK_210_BASEPATH}; CONFIGURE_EXTRAFLAGS="--enable-cairo"
	          export PKG_CONFIG_PATH="${GTK_BASEPATH}/lib/pkgconfig";;
	*) echo "Gtk version parameter must be one of gtk-2.6, gtk-2.8 or gtk-2.10"; exit;;
esac

export PATH="${PATH}:${GTK_BASEPATH}/bin"
export INCLUDE="${GTK_BASEPATH}/include"
export LIB="${GTK_BASEPATH}/lib"

GTK_VERSION=$(pkg-config --modversion gtk+-2.0 | sed 's:\([0-9]*\.[0-9]*\).[0-9]*:\1:')
GHC_VERSION=$(ghc --numeric-version)

case $3 in
	env-only) echo ${PATH}; exit;;
	*);;
esac

echo "Building Gtk2Hs ${VERSION} with GHC ${GHC_VERSION} and Gtk+ ${GTK_VERSION} ..."

VERSION_SUFFIX="ghc-${GHC_VERSION}-gtk-${GTK_VERSION}"
VERSIONED_DIR="gtk2hs-${VERSION}-${VERSION_SUFFIX}"
BUILD_DIR="build-${VERSIONED_DIR}"

rm -rf ${BUILD_DIR}
mkdir ${BUILD_DIR}
cd ${BUILD_DIR}
tar -xzf ../gtk2hs-${VERSION}.tar.gz
cd gtk2hs-${VERSION}
./configure --enable-packager-mode --enable-split-objs --enable-libglade --prefix=/ ${CONFIGURE_EXTRAFLAGS}
make
make install DESTDIR="${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}"
rm -rf ${INSTALL_SOURCE_DIR}/${VERSIONED_DIR}
mv ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}/lib/gtk2hs ${INSTALL_SOURCE_DIR}/${VERSIONED_DIR}
rmdir ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}/lib
rmdir ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}
