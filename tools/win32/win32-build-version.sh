#! /bin/sh
set -x
. ./win32-build.conf

GTK_VERSION=$(pkg-config --modversion gtk+-2.0 | sed 's:\([0-9]*\.[0-9]*\).[0-9]*:\1:')
GHC_VERSION=$(ghc --numeric-version)

echo "Building Gtk2Hs ${VERSION} with GHC ${GHC_VERSION} and Gtk+ ${GTK_VERSION} ..."

VERSION_SUFFIX="ghc-${GHC_VERSION}"
VERSIONED_DIR="gtk2hs-${VERSION}-${VERSION_SUFFIX}"
BUILD_DIR="build-${VERSIONED_DIR}"

CONFIGURE_FLAGS="--enable-packager-mode --enable-split-objs --enable-profiling --enable-docs"
ENABLE_PACKAGES="--enable-gtk --enable-libglade --enable-opengl --enable-gio --enable-gstreamer --enable-cairo --enable-svg --enable-gconf --enable-gtksourceview2"

rm -rf ${BUILD_DIR}
if [ ! -d ${BUILD_DIR} ] ; then
  mkdir ${BUILD_DIR}
  cd ${BUILD_DIR}
  tar -xzf ../gtk2hs-${VERSION}.tar.gz
else
  cd ${BUILD_DIR}
fi
cd gtk2hs-${VERSION}

PREFIX=/c/gtk2hs/$VERSION
DESTDIR="${INSTALL_SOURCE_DIR}/destdir-ghc-$GHC_VERSION"

if [ ! -f Makefile ] ; then
  XARGS="/bin/xargs -L128" ./configure --prefix=$PREFIX ${CONFIGURE_FLAGS} ${ENABLE_PACKAGES} ${CONFIGURE_EXTRAFLAGS} || exit 1
fi

make HSTOOLFLAGS=-M256m
make install DESTDIR="$DESTDIR"

rm -rf "$/ghclibs"
INSTALL_GHC_LIBS_DIR=${INSTALL_SOURCE_DIR}/gtk2hs-${VERSION}-ghc-${GHC_VERSION}
INSTALL_CLIBS_DIR=${INSTALL_SOURCE_DIR}/gtk2hs-${VERSION}-clibs
INSTALL_DOC_DIR=${INSTALL_SOURCE_DIR}/gtk2hs-${VERSION}-doc
INSTALL_DEMO_DIR=${INSTALL_SOURCE_DIR}/gtk2hs-${VERSION}-demo
mkdir -p ${INSTALL_GHC_LIBS_DIR} || exit 1
mkdir -p ${INSTALL_CLIBS_DIR} || exit 1
mkdir -p ${INSTALL_DOC_DIR} || exit 1
mkdir -p ${INSTALL_DEMO_DIR} || exit 1

cp -av $PREFIX/* ${INSTALL_CLIBS_DIR} || exit 1
mv -v ${DESTDIR}/${PREFIX}/lib ${INSTALL_GHC_LIBS_DIR} || exit 1
mkdir -p ${INSTALL_DOC_DIR}/share/doc || exit 1
mv -v ${DESTDIR}/${PREFIX}/share/doc/gtk2hs/html ${INSTALL_DOC_DIR}/share/doc || exit 1
mkdir -p ${INSTALL_DEMO_DIR}/share/demo || exit 1
cp -av ${BUILD_DIR}/gtk2hs-${VERSION}/demo/* ${INSTALL_DEMO_DIR}/share/demo/ || exit 1

rm -rf ${BUILD_DIR}
