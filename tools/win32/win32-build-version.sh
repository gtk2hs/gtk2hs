#! /bin/sh

. ./win32-build.conf

PATH=${BASE_PATH}
case $1 in
	ghc-6.8.3) PATH="${PATH}:${GHC_683_PATH}";;
	*) echo "GHC version parameter must be ghc-6.8.3"; exit;;
esac

export PATH="${PATH}:${CLIBS_BASEPATH}/bin"
export INCLUDE="${CLIBS_BASEPATH}/include"
export LIB="${CLIBS_BASEPATH}/lib"

GTK_VERSION=$(pkg-config --modversion gtk+-2.0 | sed 's:\([0-9]*\.[0-9]*\).[0-9]*:\1:')
GHC_VERSION=$(ghc --numeric-version)

case $3 in
	env-only) echo ${PATH}; exit;;
	*);;
esac

echo "Building Gtk2Hs ${VERSION} with GHC ${GHC_VERSION} and Gtk+ ${GTK_VERSION} ..."

VERSION_SUFFIX="ghc-${GHC_VERSION}"
VERSIONED_DIR="gtk2hs-${VERSION}-${VERSION_SUFFIX}"
BUILD_DIR="build-${VERSIONED_DIR}"

CONFIGURE_FLAGS="--enable-packager-mode --enable-split-objs --enable-profiling --enable-docs"
ENABLE_PACKAGES="--enable-libglade --enable-opengl --enable-gnomevfs --enable-gstreamer --enable-cairo --enable-svg --enable-gconf"

#rm -rf ${BUILD_DIR}
if [ ! -d ${BUILD_DIR} ] ; then
  mkdir ${BUILD_DIR}
  cd ${BUILD_DIR}
  tar -xzf ../gtk2hs-${VERSION}.tar.gz
else
  cd ${BUILD_DIR}
fi
cd gtk2hs-${VERSION}
if [ ! -f Makefile ] ; then
  ./configure --prefix=/ ${CONFIGURE_FLAGS} ${ENABLE_PACKAGES} ${CONFIGURE_EXTRAFLAGS}
fi
make HSTOOLFLAGS=-M256m
make install DESTDIR="${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}"
rm -rf ${INSTALL_SOURCE_DIR}/${VERSIONED_DIR}
mv ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}/lib/gtk2hs ${INSTALL_SOURCE_DIR}/${VERSIONED_DIR}
mv ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}/share/doc/gtk2hs/html ${INSTALL_SOURCE_DIR}/${VERSIONED_DIR}
rmdir ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}/lib
rmdir ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}/share{/doc{/gtk2hs,},}
rmdir ${INSTALL_SOURCE_DIR}/tmp-${VERSIONED_DIR}
