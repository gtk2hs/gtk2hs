dnl Gtk2hs - A GUI Library for Haskell based on Gtk+
dnl
dnl Copyright (c) 2001, 2002, 2003-2005 Axel Simon <as49@ukc.ac.uk>
dnl Copyright (c) 2004-2005 Duncan Coutts <duncan@coutts.uklinux.net>
dnl with parts stolen from Manuel Chakravaty, Sven Panne and Micheal Weber
dnl
dnl This library is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU Library General Public
dnl License as published by the Free Software Foundation; either
dnl version 2 of the License, or (at your option) any later version.
dnl 
dnl This library is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Library General Public License for more details.
dnl 
dnl You should have received a copy of the GNU Library General Public
dnl License along with this library (COPYING.LIB); if not, write to the Free
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

dnl ######################################################################
dnl Process this file with autoconf to produce a configure script.
dnl ######################################################################

AC_INIT([gtk2hs],[0.9.12.2])
AM_INIT_AUTOMAKE

dnl * We require autoconf version 2.59
dnl When this is bumped up to 2.61 then remove the definitions of AC_PROG_SED and
dnl AC_PROG_GREP and everthing used by these macros from acinclude.m4.
AC_PREREQ(2.59)

dnl Check system type.
AC_CANONICAL_HOST

AC_CONFIG_HEADERS([gtk2hs-config.h])
AH_TOP([
/* Hack to suppress warnings that these symbols clash with
   the ones from ghc's version of config.h */
#undef /**/ PACKAGE_NAME
#undef /**/ PACKAGE_STRING
#undef /**/ PACKAGE_TARNAME
#undef /**/ PACKAGE_VERSION
])

dnl Checks for programs.
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(AR,ar)
AC_PATH_PROG(LD,ld)
AC_PATH_PROG(BASENAME,basename)
AC_PROG_GREP
AC_PATH_PROG(GZIP,gzip)
AC_PROG_SED
AC_PATH_PROG(CUT,cut)
AC_PATH_PROG(TAR,tar)
AC_PATH_PROG(TOUCH,touch)
AC_PROG_RANLIB

dnl Does ld support -x and the @FILE input style?
PROG_LD_X
PROG_LD_INPUT
AM_CONDITIONAL(LD_INPUT, test "$LD_INPUT" = "yes")

dnl Figure out file extensions.
AC_EXEEXT

dnl Check if we're on windows
case "$host_os" in
  mingw* | *cygwin* )
    WIN32=yes
    HSCFLAGS="-C -optc-mms-bitfields"
    CREATE_TYPES="default"
    dnl we could set -optl-mwindows by default on windows, but then you
    dnl an exception if you use standard IO, hmmm.
    STARTUP_CODE=;;
  *)
    WIN32=no
    HSCFLAGS=""
    CREATE_TYPES="plugNsocket default"
    STARTUP_CODE=;;
esac
AM_CONDITIONAL(WIN32, test "$WIN32" = "yes")

dnl Select a specific Haskell compiler and/or flags.
AC_ARG_WITH(hc, 
            [  --with-hc=HC            use Haskell compiler HC],
	    [HC=$withval])

AC_ARG_WITH(hcflags, 
            [  --with-hcflags=HCFLAGS  flags for Haskell tools, default: -O],
	    [HCFLAGS=$withval],[HCFLAGS=-O])



dnl Check for GHC-6.0 or greater.
dnl (The next command is not executed, if $HC is already set.)
AC_PATH_PROG(HC, [$HC ghc], ghc-not-found)
if test $HC = ghc-not-found; then
 AC_MSG_ERROR([
Could not find GHC!  This is the only supported compiler.
You need GHC 6.0 or later.])
fi

GHC=$HC

dnl Check GHC details.
AC_MSG_CHECKING([version of GHC])
GHC_VERSION=`$GHC --numeric-version`
AC_MSG_RESULT([$GHC_VERSION])

GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -lt, 6.0, [
  AC_MSG_ERROR([Gtk2Hs requires GHC 6.0 or later])])

dnl Calculate a version number with 3 digits (i.e. 502 for 5.2)
GHC_VERSION_NUMBER=`echo $GHC_VERSION | $SED "s/[[0-9]*\.\([0-9]]*\).*/0\1/"`
GHC_VERSION_NUMBER=`echo $GHC_VERSION_NUMBER | $SED "s/[[0-9]*\([0-9][0-9]]\)/\1/"`
GHC_VERSION_NUMBER=`echo $GHC_VERSION | $SED "s/\([[0-9]]\).*/\1/"`$GHC_VERSION_NUMBER

AC_DEFINE_UNQUOTED(__GLASGOW_HASKELL__, $GHC_VERSION_NUMBER,
	[Version number of GHC.])

dnl From ghc 6.4 onwards, ghc-pkg uses a new package format so we must produce
dnl and register .package.conf files instead of the old .pkg files.
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -ge, 6.4.0, [
USE_NEW_PKG_FORMAT=yes
HAVE_DATA_TREE=yes
HAVE_MTL_PACKAGE=yes
])
AM_CONDITIONAL(USE_NEW_PKG_FORMAT, test "$USE_NEW_PKG_FORMAT" = "yes")
if test "$USE_NEW_PKG_FORMAT" = "yes"; then
  PKGEXT=package.conf
else
  PKGEXT=pkg
fi
AM_CONDITIONAL(HAVE_DATA_TREE, test "$HAVE_DATA_TREE" = "yes")
AM_CONDITIONAL(HAVE_MTL_PACKAGE, test "$HAVE_MTL_PACKAGE" = "yes")
AC_SUBST(USE_NEW_PKG_FORMAT)
AC_SUBST(HAVE_DATA_TREE)
AC_SUBST(HAVE_MTL_PACKAGE)

dnl From ghc-6.4.1 onwards, ghc supports a -hide-all-packages flag which means
dnl that we start with an empty package environment. This makes it easier to
dnl check that we are not depending on packages from the environment but have
dnl stated our package deps explicitly.
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -ge, 6.4.1, [
HIDE_ALL_PACKAGES=-hide-all-packages
])
AC_SUBST(HIDE_ALL_PACKAGES)

dnl check for a recent ghc-6.7 or later as a proxy for the base package split
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -ge, 6.7.20070912, [
HAVE_SPLIT_BASE=yes
AC_DEFINE([HAVE_SPLIT_BASE], [1], [Using split base package.])
])
AM_CONDITIONAL(HAVE_SPLIT_BASE, test "$HAVE_SPLIT_BASE" = "yes")
AC_SUBST(HAVE_SPLIT_BASE)

GHCBARE=`basename $GHC`
GHCDIR=`dirname $GHC`
GHCPKGNAME=ghc-pkg`echo $GHCBARE | $SED s/ghc//`

dnl Check for ghc-pkg. Use the one that is in the same directory and
dnl version suffix as the specified compiler.
AC_PATH_PROGS(GHCPKG, $GHCPKGNAME ghc-pkg, ghcpkg-not-found, $GHCDIR)

if test $GHCPKG = ghcpkg-not-found; then
  AC_MSG_ERROR([ghc-pkg not found. (But ghc exists!?)]);
fi

dnl Define WIN32 on win32 platform. Perhaps this should go in the
dnl gtk2hs-config.h file?
if test "$WIN32" = "yes"; then
  HCFLAGS="$HCFLAGS -DWIN32"
fi

AC_ARG_ENABLE(profiling,
	     [  --enable-profiling      additionally build profiling versions of all libs],
	     [ENABLE_PROFILING=$enableval],[ENABLE_PROFILING=no])
AM_CONDITIONAL(ENABLE_PROFILING, test "$ENABLE_PROFILING" = "yes")

AC_ARG_ENABLE(split-objs,
	     [  --enable-split-objs     produce smaller executables (on supported platforms)],
	     [ENABLE_SPLITOBJS=$enableval],[ENABLE_SPLITOBJS=yes])
AM_CONDITIONAL(ENABLE_SPLITOBJS, test "$ENABLE_SPLITOBJS" = "yes")

dnl packager mode is for people building packages, eg for linux distros.
dnl in packager mode you do not get any optional dependancies unless you
dnl explicitly ask for them with --enable-PACKAGE
dnl also in packager mode it defaults to --without-pkgreg see below.
dnl see http://www.onlamp.com/pub/a/onlamp/2005/03/31/packaging.html?page=2
AC_ARG_ENABLE([packager-mode],
	AS_HELP_STRING([--enable-packager-mode],
        		[change configuration behavior to ease packaging]),
	[if test x"${enableval}" = xyes; then
		packager_mode=yes
	else
		packager_mode=no
	fi],
	[packager_mode=no])
if test "$packager_mode" = "yes"; then
	ENABLE_PKG_DEFAULT=no
	PKGREG_DEFAULT=no
else
	ENABLE_PKG_DEFAULT=auto
	PKGREG_DEFAULT=yes
fi

dnl If we're on windows then packager mode also indicates that we want to
dnl build the installer (which means we delay some variable expansions in the
dnl package files).
AM_CONDITIONAL(WIN32_INSTALLER, test "$WIN32" = "yes" -a "$packager_mode" = "yes")

dnl Check whether to register the packages with ghc-pkg
dnl Distributors usuall want to install into a temp directory without registering
dnl anything and leave the registering phase for a post-install action.
AC_ARG_WITH(pkgreg,
            [  --without-pkgreg        Do not reigster Haskell packages (useful for distributors)],
	    [PKGREG=$withval;],[PKGREG=$PKGREG_DEFAULT;])
AM_CONDITIONAL(ENABLE_PKGREG, test "$PKGREG" = "yes")

dnl Check whether to use a local package file.
AC_ARG_WITH(user-pkgconf,
	   [  --with-user-pkgconf     use the current user's GHC package database],
	   [USERPKGCONF=yes;],[USERPKGCONF=;])

AC_ARG_WITH(pkgconf,
	   [  --with-pkgconf=FILE     GHC package file to install packages],
	   [PKGCONF=$withval;],[PKGCONF=;])


dnl Check for various GHC packages:
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -gt, 6.4.0, [GHC_PKG_CHECK(mtl)])

dnl Check for the GTK&Co libraries. Use the special PKG_CHECK_MODULES
dnl macro from the pkg-config program.
PKG_CHECK_MODULES(GLIB,[glib-2.0 >= 2.0.0 gobject-2.0 >= 2.0.0], [], AC_MSG_ERROR([

The development files for the glib-2.x library were not found.
Perhaps you need to install glib or glib-devel

The exact error reported by pkg-config was:
  "$GLIB_PKG_ERRORS"

If you installed Glib or Gtk+ in a non-standard prefix then you may
need to adjust the PKG_CONFIG_PATH environment variable.

As a last resort you may set the environment variables GLIB_CFLAGS
and GLIB_LIBS to avoid the need to call pkg-config.
See the pkg-config man page for more details.
]))
PKG_CHECK_MODULES(GTK,[pango >= 1.0.0 gdk-2.0 >= 2.0.0 gdk-pixbuf-2.0 >= 0.12.0 gtk+-2.0 >= 2.0.0 gthread-2.0 >= 2.0.0], [], AC_MSG_ERROR([

The development files for the gtk-2.x library were not found.
Perhaps you need to install gtk or gtk-devel.

The exact error reported by pkg-config was:
  "$GTK_PKG_ERRORS"

If you installed Gtk+ in a non-standard prefix then you may need to
adjust the PKG_CONFIG_PATH environment variable.

As a last resort you may set the environment variables GTK_CFLAGS
and GTK_LIBS to avoid the need to call pkg-config.
See the pkg-config man page for more details.
]))

dnl Find the Gtk installation prefix which is used in packager mode on windows
dnl for preparing the package files.
GTK_BASEPATH=`$PKG_CONFIG --variable=prefix gtk+-2.0`
AC_SUBST(GTK_BASEPATH)

dnl Check if user wants various optional bindings.
dnl Defaults to auto, or in packager mode it defaults to no.
GTKHS_PKG_CHECK(libglade, glade, LIBGLADE, [libglade-2.0 >= 2.0.0],
	[build glade package (default=auto)],
	[libglade library requirement not met. Perhaps you need to install libglade or libglade-devel])

GTKHS_PKG_CHECK(gconf, gconf, GCONF, [gconf-2.0 >= 2.0.0],
	[build gconf package (default=auto)],
	[GConf requirement not met. Perhaps you need to install gconf or gconf-devel])

GTKHS_PKG_CHECK(sourceview, sourceview, SOURCEVIEW, [gtksourceview-1.0 >= 0.6.0],
	[build sourceview package (default=auto)],
	[GtkSourceView requirement not met. Perhaps you need to install gtksourceview or gtksourceview-devel])

GTKHS_PKG_CHECK(mozilla, mozembed, MOZILLA_MOZEMBED, [mozilla-gtkmozembed >= 1.4],
	[build bindings for the Mozilla display widget (default=auto)],
	[Mozilla requirement not met. Perhaps you need to install mozilla or mozilla-devel])
MOZILLA_MOZEMBED_LIBDIR=$(${PKG_CONFIG} --variable=libdir mozilla-gtkmozembed)

GTKHS_PKG_CHECK(seamonkey, mozembed, SEAMONKEY_MOZEMBED, [seamonkey-gtkmozembed >= 1.0],
        [build bindings for the Mozilla display widget using the Seamonkey libs (default=auto)],
        [Seamonkey requirement not met. Perhaps you need to install seamonkey or seamonkey-devel])
SEAMONKEY_MOZEMBED_LIBDIR=$(${PKG_CONFIG} --variable=libdir seamonkey-gtkmozembed)

GTKHS_PKG_CHECK(firefox, mozembed, FIREFOX_MOZEMBED, [firefox-gtkmozembed >= 1.0],
	[build bindings for the Mozilla display widget using the Firefox libs (default=auto)],
	[Mozilla Firefox requirement not met. Perhaps you need to install firefox or firefox-devel])
FIREFOX_MOZEMBED_LIBDIR=$(${PKG_CONFIG} --variable=libdir firefox-gtkmozembed)

GTKHS_PKG_CHECK(xulrunner, mozembed, XULRUNNER_MOZEMBED, [xulrunner-gtkmozembed >= 1.8.0.1],
	[build bindings for the Mozilla display widget using the XULRunner libs (default=auto)],
	[XULRunner requirement not met. Perhaps you need to install xulrunner or xulrunner-devel])
XULRUNNER_MOZEMBED_LIBDIR=$(${PKG_CONFIG} --variable=libdir xulrunner-gtkmozembed)

dnl Use in this order of preference: xulrunner, seamonkey, firefox, mozilla
if test "$ENABLE_XULRUNNER_MOZEMBED" = "yes"; then
        ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$XULRUNNER_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$XULRUNNER_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$XULRUNNER_MOZEMBED_LIBDIR"
elif test "$ENABLE_SEAMONKEY_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$SEAMONKEY_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$SEAMONKEY_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$SEAMONKEY_MOZEMBED_LIBDIR"
elif test "$ENABLE_FIREFOX_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$FIREFOX_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$FIREFOX_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$FIREFOX_MOZEMBED_LIBDIR"
elif test "$ENABLE_MOZILLA_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$MOZILLA_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$MOZILLA_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$MOZILLA_MOZEMBED_LIBDIR"
else
	ENABLE_MOZEMBED="no"
fi
AC_DEFINE_UNQUOTED(MOZEMBED_LIBDIR,"$MOZEMBED_LIBDIR",[The mozembed library directory])
AM_CONDITIONAL(ENABLE_MOZEMBED, test "$ENABLE_MOZEMBED" = "yes")
AC_SUBST(MOZEMBED_CFLAGS)
AC_SUBST(MOZEMBED_LIBS)

GTKHS_PKG_CHECK(cairo, cairo, CAIRO, [cairo >= 1.0.0],
	[build bindings for the cairo vector graphics system (default=auto)],
	[cairo requirement not met. Perhaps you need to install cairo or cairo-devel])

GTKHS_PKG_CHECK(svg, svgcairo, SVGCAIRO, [librsvg-2.0 >= 2.16.0],
	[build bindings for the cairo svg extension (default=auto)],
	[librsvg requirement not met. Perhaps you need to install librsvg or librsvg-devel])

GTKHS_PKG_CHECK(opengl, gtkglext, GTKGLEXT, [gtkglext-1.0 >= 1.0.5],
 	[build bindings for the Gtk OpenGL extension (default=auto)],
 	[GtkGLExt library requirement not met. Perhaps you need to install gtkglext or gtkglext-devel])

GTKHS_PKG_CHECK(gnomevfs, gnomevfs, GNOMEVFS, [gnome-vfs-2.0 >= 2.0.0],
	[build gnome-vfs package (default=auto)],
	[gnomevfs library requirement not met. Perhaps you need to install libgnomevfs or libgnomevfs-devel])

GTKHS_PKG_CHECK(gstreamer, gstreamer, GSTREAMER,
                [gstreamer-0.10 >= 0.10 \
                 gstreamer-base-0.10 >= 0.10 \
                 gstreamer-controller-0.10 >= 0.10 
                 gstreamer-dataprotocol-0.10 >= 0.10 \
                 gstreamer-net-0.10 >= 0.10 \
                 gstreamer-plugins-base-0.10],
	[build gstreamer package (default=auto)],
	[gstreamer library requirement not met. Perhaps you need to install libgstreamer or libgstreamer-devel])

dnl The following is a hack to fix the fact that
dnl "pkg-config --libs mozilla-gtkmozembed" does not always give us all the
dnl right stuff. On many systems, the mozilla .so files that we need to link
dnl with are not installed on the standard library search path.
dnl "pkg-config --libs mozilla-gtkmozembed" tells us -L/path/to/moz/libs which
dnl helps with finding the .so libs at static link time but does nothing to
dnl modify the search path at dynamic link/load time. Hence what we do here is
dnl modify the lib flags to add -rpath${mozlibdir} if it is not there already.

if ! echo ${MOZEMBED_LIBS} | $GREP -e "-Wl,-\(rpath\|R\),\?${MOZEMBED_LIBDIR}" > /dev/null; then
	MOZEMBED_LIBS="${MOZEMBED_LIBS} -Wl,-rpath,${MOZEMBED_LIBDIR}"
fi

dnl Another hack, on glibc systems GHCi cannot load the pthread library,
dnl so do not include it in the LIBS list. This is not usually a problem since
dnl some other lib usually has the pthread library as a dependency and the
dnl system dynamic linker loads up the pthread library automatically.
case "$host_os" in
  linux*)
    GTKHS_GLIBC_PTHREAD_HACK(LIBGLADE_LIBS)
    GTKHS_GLIBC_PTHREAD_HACK(GCONF_LIBS)
    GTKHS_GLIBC_PTHREAD_HACK(SOURCEVIEW_LIBS)
    GTKHS_GLIBC_PTHREAD_HACK(MOZEMBED_LIBS);;
esac

dnl Some APIs only appeared in later versions of libraries. Generate only
dnl Haskell types for the available C types.
HAVE_GTK_VERSION_2_2=`$PKG_CONFIG gtk+-2.0 --atleast-version=2.2 && echo yes || echo no`
HAVE_GTK_VERSION_2_4=`$PKG_CONFIG gtk+-2.0 --atleast-version=2.4 && echo yes || echo no`
HAVE_GTK_VERSION_2_6=`$PKG_CONFIG gtk+-2.0 --atleast-version=2.6 && echo yes || echo no`
HAVE_GTK_VERSION_2_8=`$PKG_CONFIG gtk+-2.0 --atleast-version=2.8 && echo yes || echo no`
HAVE_GTK_VERSION_2_10=`$PKG_CONFIG gtk+-2.0 --atleast-version=2.10 && echo yes || echo no`
HAVE_GTK_VERSION_2_12=`$PKG_CONFIG gtk+-2.0 --atleast-version=2.12 && echo yes || echo no`

AM_CONDITIONAL(HAVE_GTK_VERSION_2_2, test "$HAVE_GTK_VERSION_2_2" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_4, test "$HAVE_GTK_VERSION_2_4" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_6, test "$HAVE_GTK_VERSION_2_6" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_8, test "$HAVE_GTK_VERSION_2_8" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_10, test "$HAVE_GTK_VERSION_2_10" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_12, test "$HAVE_GTK_VERSION_2_12" = "yes")

CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_2" = "yes" && echo gtk-2.2`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_4" = "yes" && echo gtk-2.4`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_6" = "yes" && echo gtk-2.6`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_8" = "yes" && echo gtk-2.8`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_10" = "yes" && echo gtk-2.10`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_12" = "yes" && echo gtk-2.12`"

dnl Also allow us to conditionally compile binding to the new Gtk+ APIs.
GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`
GTK_MAJOR_VERSION=`echo $GTK_VERSION | $CUT -d. -f1`
GTK_MINOR_VERSION=`echo $GTK_VERSION | $CUT -d. -f2`
GTK_MICRO_VERSION=`echo $GTK_VERSION | $CUT -d. -f3`
AC_DEFINE_UNQUOTED(_GTK_MAJOR_VERSION, ($GTK_MAJOR_VERSION), [Gtk major version number])
AC_DEFINE_UNQUOTED(_GTK_MINOR_VERSION, ($GTK_MINOR_VERSION), [Gtk minor version number])
AC_DEFINE_UNQUOTED(_GTK_MICRO_VERSION, ($GTK_MICRO_VERSION), [Gtk minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of GTK+ */
#define GTK_CHECK_VERSION(major,minor,micro)    \
    (_GTK_MAJOR_VERSION > (major) || \
     (_GTK_MAJOR_VERSION == (major) && _GTK_MINOR_VERSION > (minor)) || \
     (_GTK_MAJOR_VERSION == (major) && _GTK_MINOR_VERSION == (minor) && \
      _GTK_MICRO_VERSION >= (micro)))
])

dnl Also allow us to conditionally compile bindings to Pango APIs.
PANGO_VERSION=`$PKG_CONFIG pango --modversion`
PANGO_MAJOR_VERSION=`echo $PANGO_VERSION | $CUT -d. -f1`
PANGO_MINOR_VERSION=`echo $PANGO_VERSION | $CUT -d. -f2`
PANGO_MICRO_VERSION=`echo $PANGO_VERSION | $CUT -d. -f3`
AC_DEFINE_UNQUOTED(_PANGO_MAJOR_VERSION, ($PANGO_MAJOR_VERSION), [Pango major version number])
AC_DEFINE_UNQUOTED(_PANGO_MINOR_VERSION, ($PANGO_MINOR_VERSION), [Pango minor version number])
AC_DEFINE_UNQUOTED(_PANGO_MICRO_VERSION, ($PANGO_MICRO_VERSION), [Pango minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of Pango */
#define PANGO_CHECK_VERSION(major,minor,micro)    \
    (_PANGO_MAJOR_VERSION > (major) || \
     (_PANGO_MAJOR_VERSION == (major) && _PANGO_MINOR_VERSION > (minor)) || \
     (_PANGO_MAJOR_VERSION == (major) && _PANGO_MINOR_VERSION == (minor) && \
      _PANGO_MICRO_VERSION >= (micro)))
])

dnl Also allow us to conditionally compile bindings to Glib APIs.
GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`
GLIB_MAJOR_VERSION=`echo $GLIB_VERSION | $CUT -d. -f1`
GLIB_MINOR_VERSION=`echo $GLIB_VERSION | $CUT -d. -f2`
GLIB_MICRO_VERSION=`echo $GLIB_VERSION | $CUT -d. -f3`
AC_DEFINE_UNQUOTED(_GLIB_MAJOR_VERSION, ($GLIB_MAJOR_VERSION), [Glib major version number])
AC_DEFINE_UNQUOTED(_GLIB_MINOR_VERSION, ($GLIB_MINOR_VERSION), [Glib minor version number])
AC_DEFINE_UNQUOTED(_GLIB_MICRO_VERSION, ($GLIB_MICRO_VERSION), [Glib minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of Glib */
#define GLIB_CHECK_VERSION(major,minor,micro)    \
    (_GLIB_MAJOR_VERSION > (major) || \
     (_GLIB_MAJOR_VERSION == (major) && _GLIB_MINOR_VERSION > (minor)) || \
     (_GLIB_MAJOR_VERSION == (major) && _GLIB_MINOR_VERSION == (minor) && \
      _GLIB_MICRO_VERSION >= (micro)))
])

dnl Also allow us to conditionally compile bindings to Glib APIs.
dnl We also want to know if certain cairo backends are enabled or not
dnl we can find out by checking the appropriate cpp defines in cairo-features.h
dnl Actually getting at these cpp defines is a bit tricky, what we do is
dnl pre-process a shell file and include cairo-features.h then we run that
dnl shell file to bring in the values of the shell vars it defines.
dnl Then finally we substitute those into our config header file.
CAIRO_CPP_FLAGS="`$PKG_CONFIG --cflags cairo` -include cairo-features.h"
$CPP $CAIRO_CPP_FLAGS - > conftest.sh <<_ACEOF
#ifdef CAIRO_HAS_PS_SURFACE
_CAIRO_HAS_PS_SURFACE=1
#endif
#ifdef CAIRO_HAS_PDF_SURFACE
_CAIRO_HAS_PDF_SURFACE=1
#endif
#ifdef CAIRO_HAS_SVG_SURFACE
_CAIRO_HAS_SVG_SURFACE=1
#endif
#ifdef CAIRO_HAS_PNG_FUNCTIONS
_CAIRO_HAS_PNG_FUNCTIONS=1
#endif
_CAIRO_VERSION_MAJOR=CAIRO_VERSION_MAJOR
_CAIRO_VERSION_MINOR=CAIRO_VERSION_MINOR
_CAIRO_VERSION_MICRO=CAIRO_VERSION_MICRO
_ACEOF
. ./conftest.sh
CAIRO_HEADERS="\"cairo.h\""
if test ${_CAIRO_HAS_PS_SURFACE}; then
AC_DEFINE_UNQUOTED(ENABLE_CAIRO_PS_SURFACE, [], [cairo ps backend enabled])
CAIRO_HEADERS="${CAIRO_HEADERS}, \"cairo-ps.h\""
fi
if test ${_CAIRO_HAS_PDF_SURFACE}; then
AC_DEFINE_UNQUOTED(ENABLE_CAIRO_PDF_SURFACE, [], [cairo pdf backend enabled])
CAIRO_HEADERS="${CAIRO_HEADERS}, \"cairo-pdf.h\""
fi
if test ${_CAIRO_HAS_SVG_SURFACE}; then
AC_DEFINE_UNQUOTED(ENABLE_CAIRO_SVG_SURFACE, [], [cairo svg backend enabled])
CAIRO_HEADERS="${CAIRO_HEADERS}, \"cairo-svg.h\""
fi
if test ${_CAIRO_HAS_PNG_FUNCTIONS}; then
AC_DEFINE_UNQUOTED(ENABLE_CAIRO_PNG_FUNCTIONS, [], [cairo png functions available])
fi
AC_DEFINE_UNQUOTED(_CAIRO_MAJOR_VERSION, (${_CAIRO_VERSION_MAJOR}), [cairo major version number])
AC_DEFINE_UNQUOTED(_CAIRO_MINOR_VERSION, (${_CAIRO_VERSION_MINOR}), [cairo minor version number])
AC_DEFINE_UNQUOTED(_CAIRO_MICRO_VERSION, (${_CAIRO_VERSION_MICRO}), [cairo minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of cairo */
#define CAIRO_CHECK_VERSION(major,minor,micro)    \
    (_CAIRO_MAJOR_VERSION > (major) || \
     (_CAIRO_MAJOR_VERSION == (major) && _CAIRO_MINOR_VERSION > (minor)) || \
     (_CAIRO_MAJOR_VERSION == (major) && _CAIRO_MINOR_VERSION == (minor) && \
      _CAIRO_MICRO_VERSION >= (micro)))
])


dnl For reasons know only to themselves, Sun decided to ship a development
dnl version of Gtk+ (version 2.1.0). Being an intermediate development version
dnl it had bugs/typos etc. The function gtk_icon_size_lookup was mistakenly
dnl conditionally included based on the preprocessor symbol GTK_MULTIHEAD_SAFE.
dnl We'll be nice and support Sun's poor users by defining this symbol for this
dnl version of Gtk+ only.
if $PKG_CONFIG gtk+-2.0 --exact-version 2.1.0; then
  AC_DEFINE(GTK_MULTIHEAD_SAFE, [],
  [Define this if we need to hack around a broken development version of Gtk+ (2.1.0)
  that for some reason Sun decided it'd be a really good idea for them to ship. Grrr.])
fi

dnl If we're building with Gtk+ 2.8 or later then we also depend on the cairo
dnl bindings for Haskell. So set the GTK_CAIRO_DEPEND variable to the
dnl appropaiate value to substitute into the gtk package files

if test "$HAVE_GTK_VERSION_2_8" = "yes" -a "$ENABLE_CAIRO" = "yes"; then
  GTK_CAIRO_DEPEND=", cairo-${PACKAGE_VERSION}";
  GTK_CAIRO_DEPEND_NO_CABAL=",\"cairo\"";
fi

dnl The GtkPlug/GtkSocket widgets have been ported to Win32 as of Gtk+ 2.8.0
if test "$HAVE_GTK_VERSION_2_8" = "yes" -a "$WIN32" = "yes"; then
  CREATE_TYPES="$CREATE_TYPES plugNsocket"
fi

dnl The configuration program for GTK is kind of stupid in that it
dnl lists directories which don't exist. ghc-pkg in ghc 5.04 or greater
dnl does not like that, so we need to filter out non-existent directories.
dnl Furthermore we remove all libraries and directories for packages that
dnl built on top of gtk so that they don't appear several times on the
dnl command line.
GTKHS_PACKAGE_FILTER_CFLAGS([GLIB_CFLAGS], [], [$GLIB_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([GLIB_LIBS], [], [$GLIB_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([GTK_CFLAGS], [$GLIB_CFLAGS], [$GTK_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([GTK_LIBS], [$GLIB_LIBS], [$GTK_LIBS $STARTUP_CODE])

GTKHS_PACKAGE_FILTER_CFLAGS([SOURCEVIEW_CFLAGS], [$GLIB_CFLAGS $GTK_CFLAGS], [$SOURCEVIEW_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([SOURCEVIEW_LIBS], [$GLIB_LIBS $GTK_LIBS], [$SOURCEVIEW_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([LIBGLADE_CFLAGS], [$GLIB_CFLAGS $GTK_CFLAGS], [$LIBGLADE_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([LIBGLADE_LIBS], [$GLIB_LIBS $GTK_LIBS], [$LIBGLADE_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([GCONF_CFLAGS], [$GLIB_CFLAGS], [$GCONF_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([GCONF_LIBS], [$GLIB_LIBS], [$GCONF_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([MOZEMBED_CFLAGS], [$GLIB_CFLAGS $GTK_CFLAGS], [$MOZEMBED_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([MOZEMBED_LIBS], [$GLIB_LIBS $GTK_LIBS], [$MOZEMBED_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([CAIRO_CFLAGS], [], [$CAIRO_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([CAIRO_LIBS], [], [$CAIRO_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([SVGCAIRO_CFLAGS], [$CAIRO_CFLAGS], [$SVGCAIRO_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([SVGCAIRO_LIBS], [$CAIRO_LIBS], [$SVGCAIRO_LIBS])

GTKHS_PACKAGE_FILTER_CFLAGS([GTKGLEXT_CFLAGS], [$GLIB_CFLAGS $GTK_CFLAGS], [$GTKGLEXT_CFLAGS])
GTKHS_PACKAGE_FILTER_LIBS([GTKGLEXT_LIBS], [$GLIB_LIBS $GTK_LIBS], [$GTKGLEXT_LIBS])


dnl Change the representation of these flags to "flag1","flag2". The
dnl letters CQ stand for Comma, Quote.
GTKHS_REFORMAT_PACKAGE_CFLAGS(GLIB_CFLAGS, GLIB_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(GLIB_LIBS, GLIB_LIBS_CQ, GLIB_LIBDIR_CQ, GLIB_LIBEXTRA_CQ)
AC_SUBST(GLIB_CFLAGS_CQ)
AC_SUBST(GLIB_LIBS_CQ)
AC_SUBST(GLIB_LIBDIR_CQ)
AC_SUBST(GLIB_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(GTK_CFLAGS, GTK_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(GTK_LIBS, GTK_LIBS_CQ, GTK_LIBDIR_CQ, GTK_LIBEXTRA_CQ)
AC_SUBST(GTK_CFLAGS_CQ)
AC_SUBST(GTK_LIBS_CQ)
AC_SUBST(GTK_LIBDIR_CQ)
AC_SUBST(GTK_LIBEXTRA_CQ)

dnl Fix another bug in ghc-pkg where it doesn't look into directories
dnl for dependant packages when building the .o file.
GTKHS_REFORMAT_PACKAGE_CFLAGS(SOURCEVIEW_CFLAGS, SOURCEVIEW_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(SOURCEVIEW_LIBS, SOURCEVIEW_LIBS_CQ,
			    SOURCEVIEW_LIBDIR_CQ, SOURCEVIEW_LIBEXTRA_CQ)
AC_SUBST(SOURCEVIEW_CFLAGS_CQ)
AC_SUBST(SOURCEVIEW_LIBS_CQ)
AC_SUBST(SOURCEVIEW_LIBDIR_CQ)
AC_SUBST(SOURCEVIEW_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(LIBGLADE_CFLAGS, LIBGLADE_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(LIBGLADE_LIBS, LIBGLADE_LIBS_CQ,
			    LIBGLADE_LIBDIR_CQ, LIBGLADE_LIBEXTRA_CQ)
AC_SUBST(LIBGLADE_CFLAGS_CQ)
AC_SUBST(LIBGLADE_LIBS_CQ)
AC_SUBST(LIBGLADE_LIBDIR_CQ)
AC_SUBST(LIBGLADE_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(GCONF_CFLAGS, GCONF_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(GCONF_LIBS, GCONF_LIBS_CQ, GCONF_LIBDIR_CQ,
			    GCONF_LIBEXTRA_CQ)
AC_SUBST(GCONF_CFLAGS_CQ)
AC_SUBST(GCONF_LIBS_CQ)
AC_SUBST(GCONF_LIBDIR_CQ)
AC_SUBST(GCONF_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(MOZEMBED_CFLAGS, MOZEMBED_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(MOZEMBED_LIBS, MOZEMBED_LIBS_CQ,
                            MOZEMBED_LIBDIR_CQ, MOZEMBED_LIBEXTRA_CQ)
AC_SUBST(MOZEMBED_CFLAGS_CQ)
AC_SUBST(MOZEMBED_LIBS_CQ)
AC_SUBST(MOZEMBED_LIBDIR_CQ)
AC_SUBST(MOZEMBED_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(CAIRO_CFLAGS, CAIRO_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(CAIRO_LIBS, CAIRO_LIBS_CQ, CAIRO_LIBDIR_CQ, CAIRO_LIBEXTRA_CQ)
AC_SUBST(CAIRO_CFLAGS_CQ)
AC_SUBST(CAIRO_LIBS_CQ)
AC_SUBST(CAIRO_LIBDIR_CQ)
AC_SUBST(CAIRO_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(SVGCAIRO_CFLAGS, SVGCAIRO_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(SVGCAIRO_LIBS, SVGCAIRO_LIBS_CQ, SVGCAIRO_LIBDIR_CQ, SVGCAIRO_LIBEXTRA_CQ)
AC_SUBST(SVGCAIRO_CFLAGS_CQ)
AC_SUBST(SVGCAIRO_LIBS_CQ)
AC_SUBST(SVGCAIRO_LIBDIR_CQ)
AC_SUBST(SVGCAIRO_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(GTKGLEXT_CFLAGS, GTKGLEXT_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(GTKGLEXT_LIBS, GTKGLEXT_LIBS_CQ, GTKGLEXT_LIBDIR_CQ, GTKGLEXT_LIBEXTRA_CQ)
AC_SUBST(GTKGLEXT_CFLAGS_CQ)
AC_SUBST(GTKGLEXT_LIBS_CQ)
AC_SUBST(GTKGLEXT_LIBDIR_CQ)
AC_SUBST(GTKGLEXT_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(GNOMEVFS_CFLAGS, GNOMEVFS_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(GNOMEVFS_LIBS, GNOMEVFS_LIBS_CQ, GNOMEVFS_LIBDIR_CQ, GNOMEVFS_LIBEXTRA_CQ)
AC_SUBST(GNOMEVFS_CFLAGS_CQ)
AC_SUBST(GNOMEVFS_LIBS_CQ)
AC_SUBST(GNOMEVFS_LIBDIR_CQ)
AC_SUBST(GNOMEVFS_LIBEXTRA_CQ)

GTKHS_REFORMAT_PACKAGE_CFLAGS(GSTREAMER_CFLAGS, GSTREAMER_CFLAGS_CQ)
GTKHS_REFORMAT_PACKAGE_LIBS(GSTREAMER_LIBS, GSTREAMER_LIBS_CQ, GSTREAMER_LIBDIR_CQ, GSTREAMER_LIBEXTRA_CQ)
AC_SUBST(GSTREAMER_CFLAGS_CQ)
AC_SUBST(GSTREAMER_LIBS_CQ)
AC_SUBST(GSTREAMER_LIBDIR_CQ)
AC_SUBST(GSTREAMER_LIBEXTRA_CQ)


dnl Check if user wants bindings for deprecated APIs. Defaults to yes.
AC_MSG_CHECKING([whether to build deprecated bindings])

AC_ARG_ENABLE(deprecated,
	     [  --disable-deprecated    do not generate bindings for any deprecated APIs],
	     [ENABLE_DEPRECATED=$enableval],[ENABLE_DEPRECATED=yes])
AC_MSG_RESULT($ENABLE_DEPRECATED)

DISABLE_DEPRECATED=`test $ENABLE_DEPRECATED = yes && echo no || echo yes`

if test $DISABLE_DEPRECATED = yes; then
  AC_DEFINE(DISABLE_DEPRECATED, [], [Leave out all deprecated functions.])
  AC_DEFINE(G_DISABLE_DEPRECATED, [], [Omit deprecated glib functions.])
  AC_DEFINE(GDK_DISABLE_DEPRECATED, [], [Omit deprecated gdk functions.])
  AC_DEFINE(GDK_PIXBUF_DISABLE_DEPRECATED, [],
	[Omit deprecated pixbuf functions.])
  AC_DEFINE(GTK_DISABLE_DEPRECATED, [], [Omit deprecated gtk functions.])
else
  CREATE_TYPES="deprecated $CREATE_TYPES";
fi;

dnl Decide which signals implementation to use, it does not work with GHCi for
dnl version of GHC up to 6.4, it is fixed in 6.4.1. However we will use it on
dnl sparc and amd64 since amd64 does not have a working ghci, and its ffi
dnl support is insufficient for the old signals method. For sparc, the ghc ffi
dnl support is also insufficient for the old signals method.
case "$host_cpu" in
  sparc*) USE_GCLOSUE_SIGNALS_IMPL=yes;;
 x86_64*) USE_GCLOSUE_SIGNALS_IMPL=yes;;
       *) USE_GCLOSUE_SIGNALS_IMPL=no;;
esac
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -ge, 6.4.1, [USE_GCLOSUE_SIGNALS_IMPL=yes])
if test $USE_GCLOSUE_SIGNALS_IMPL = yes; then
	AC_DEFINE(USE_GCLOSUE_SIGNALS_IMPL, [], [Use the new GClosure based signals implementation.])
fi
AM_CONDITIONAL(USE_GCLOSUE_SIGNALS_IMPL, test "$USE_GCLOSUE_SIGNALS_IMPL" = "yes")

dnl The following is a check that is only needed by gtk2hs cvs users who
dnl upgrade from an earlier version which used to use "config.h" rather than
dnl "gtk2hs-config.h". The "config.h" file sometimes clashed with the file of
dnl the same name from ghc. In particular it can cause the alex lexer to be
dnl misconpiled on big endian machines. So we want to make sure that the file
dnl does not exist. If it does we'll ask the user to remove it.
if test -f ${srcdir}/config.h; then
  AC_MSG_ERROR([You appear to be upgrading Gtk2Hs without starting with a
		clean build tree. That's ok! But can you please delete the
		old config.h and config.h.in files, since their presense can
		cause build problems that are hard to track down.
		After deleteing the two files you can re-run ./configure])
fi

dnl Decide what default memory limits to use with the build tools (c2hs).
dnl We generally want to keep them reasonably low without cutting it too close
dnl to the absolute minimum.
if test "$WIN32" = "yes"; then
  dnl It seems to take considerably more memory on win32. Not sure why.
  dnl Don't use -H{x}m in combination with -M{x}m on windows since it seems to
  dnl trigger wierd ghc rts behaviour.
  HSTOOLFLAGS="-M120m"
else
  dnl I'll actually work in just 68Mb so 80Mb should be fine and still allow it
  dnl to run on 32bit 128Mb machines. 64bit machines need more obviously.
  case $ac_cv_sizeof_void_p in
	4) HSTOOLFLAGS="-H50m -M80m";;
	8) HSTOOLFLAGS="-H100m -M160m";;
  esac
fi

dnl Always use the local c2hs.
C2HS='./tools/c2hs/c2hsLocal$(EXEEXT)'

dnl The hsc2hs preprocessor
AC_PATH_PROG(HSC2HS,hsc2hs, no)
if test "$HSC2HS" = "no"; then
  AC_MSG_ERROR([Could not find hsc2hs. This should be installed with GHC!])
fi

dnl The happy parser generator
AC_PATH_PROG(HAPPY,happy, no)
if test "$HAPPY" = no; then
  HAPPY_ERROR="Could not find happy which is needed to build c2hs."
else
  AC_MSG_CHECKING([version of happy])
  HAPPY_VERSION=`$HAPPY --version | $SED "s/[[^0-9.]*\([0-9.]*\) .*]/\1/"`;
  AC_MSG_RESULT([$HAPPY_VERSION])
  GTKHS_PROG_CHECK_VERSION($HAPPY_VERSION, -lt, 1.15.0, [
    HAPPY_ERROR="Version 1.15 or later of happy is required."])
fi
if test -n "$HAPPY_ERROR"; then
  dnl We only need to complain if we would actually have to invoke happy
  dnl which would not be the case for a normal distribution tarball in which
  dnl the parser .hs file exists and is newer then the parser .y file
  CParser=${srcdir}/tools/c2hs/c/CParser
  if test ! -f ${CParser}.hs -o ${CParser}.hs -ot ${CParser}.y; then
    AC_MSG_ERROR([$HAPPY_ERROR])
  fi
fi

dnl The alex lexer generator
AC_PATH_PROG(ALEX,alex, no)
if test "$ALEX" = no; then
  dnl We only need to complain if we would actually have to invoke alex
  dnl which would not be the case for a normal distribution tarball in which
  dnl the lexer .hs file exists and is newer then the lexer .x file
  CLexer=${srcdir}/tools/c2hs/c/CLexer
  if test ! -f ${CLexer}.hs -o ${CLexer}.hs -ot ${CLexer}.x; then
    AC_MSG_ERROR([Could not find alex which is needed to build c2hs.])
  fi
fi

dnl Documentation
AC_ARG_ENABLE(docs,
              [  --enable-docs           build html documentation],
              [BUILDDOCS=$enableval],[BUILDDOCS=no])
if test "$BUILDDOCS" = "yes"; then
  AC_PATH_PROG(HADDOCK,haddock, no)
  if test "$HADDOCK" = "no"; then
    AC_MSG_ERROR([Could not find haddock which is required to build the documentation.
    Install haddock or re-run configure without --enable-docs])
  fi
  AC_MSG_CHECKING([version of haddock])
  HADDOCK_VERSION=`$HADDOCK --version | $CUT -d' ' -f3 | $CUT -d, -f1`
  AC_MSG_RESULT([$HADDOCK_VERSION])  
  GTKHS_PROG_CHECK_VERSION($HADDOCK_VERSION, -lt, 0.7, [
    AC_MSG_ERROR([I need haddock 0.7 or later to build the documentation.
    Upgrade your version of haddock or re-run configure without --enable-docs])])
  GTKHS_PROG_CHECK_VERSION($HADDOCK_VERSION, -ge, 0.8, [USE_CPP_LINES="yes";])
else
  USE_CPP_LINES="yes"
fi
AM_CONDITIONAL(BUILDDOCS, test "$BUILDDOCS" = "yes")

dnl Define the set of packages that Haddock should cross-reference into. This
dnl should include all modules of GHC that contain the definitions of e.g.
dnl Maybe and Bool, etc. This setting is likely dependent on the GHC version.
if test "$HAVE_MTL_PACKAGE" = "yes"; then
HADDOCK_PACKAGES="base mtl"
else
HADDOCK_PACKAGES="base"
fi

dnl The preprocessor to use on Haskell sources
if test "$USE_CPP_LINES" = "yes"; then
HSCPP='$(CPP) -x c -traditional-cpp'
else
HSCPP='$(CPP) -x c -traditional-cpp -P'
fi

dnl Needed substitution.
AC_SUBST(BUILT_IN_C2HS)
AC_SUBST(MULTIPLE_CHS)
AC_SUBST(USERPKGCONF)
AC_SUBST(PKGCONF)
AC_SUBST(PKGEXT)
AC_SUBST(HCFLAGS)
AC_SUBST(C2HS)
AC_SUBST(HSC2HS)
AC_SUBST(HSCPP)
AC_SUBST(VERSION)
dnl Platform specific flags 
AC_SUBST(HSCFLAGS)
AC_SUBST(C2HSFLAGS)
AC_SUBST(HSTOOLFLAGS)
dnl Versionitis
AC_SUBST(CREATE_TYPES)
dnl Optional packages
AC_SUBST(GTK_CAIRO_DEPEND)
AC_SUBST(GTK_CAIRO_DEPEND_NO_CABAL)
AC_SUBST(CAIRO_HEADERS)
dnl Documentation
AC_SUBST(HADDOCK)
AC_SUBST(HADDOCK_PACKAGES)
dnl The c2hs part.
AC_SUBST(CPP)

dnl write the results...
AC_CONFIG_FILES([
  Makefile
])

AC_CONFIG_FILES([
  tools/c2hs/toplevel/C2HSConfig.hs
  gtk2hs.spec
  mk/chsDepend
  mk/link-splitobjs.sh
  docs/prologue.txt

  glib/glib.pkg
  glib/glib.package.conf
  glib/glib.cabal
  
  gtk/gtk.pkg
  gtk/gtk.package.conf
  gtk/gtk.cabal
  
  glade/glade.pkg
  glade/glade.package.conf
  glade/glade.cabal
  
  gconf/gconf.pkg
  gconf/gconf.package.conf
  gconf/gconf.cabal
  
  sourceview/sourceview.pkg
  sourceview/sourceview.package.conf
  sourceview/sourceview.cabal

  mozembed/mozembed.pkg
  mozembed/mozembed.package.conf
  mozembed/mozembed.cabal
  
  cairo/cairo.pkg
  cairo/cairo.package.conf
  cairo/cairo.cabal

  svgcairo/svgcairo.pkg
  svgcairo/svgcairo.package.conf
  svgcairo/svgcairo.cabal

  gtkglext/gtkglext.pkg
  gtkglext/gtkglext.package.conf
  gtkglext/gtkglext.cabal

  soegtk/soegtk.pkg
  soegtk/soegtk.package.conf
  soegtk/soegtk.cabal

  gnomevfs/gnomevfs.pkg
  gnomevfs/gnomevfs.package.conf
  gnomevfs/gnomevfs.cabal

  gstreamer/gstreamer.pkg
  gstreamer/gstreamer.package.conf
  gstreamer/gstreamer.cabal
  ])
AC_CONFIG_COMMANDS([default],[[chmod a+x mk/chsDepend mk/link-splitobjs.sh install-sh]])
AC_OUTPUT

dnl ...and chat with the user
echo "**************************************************"
echo "* Configuration completed successfully.           "
echo "*                                                 "
echo "* The following packages will be built:           "
echo "*                                                 "
echo "* glib          : yes                             "
echo "* gtk           : yes                             "
echo "* glade         : ${ENABLE_LIBGLADE}              "
echo "* cairo         : ${ENABLE_CAIRO}                 "
echo "* svgcairo      : ${ENABLE_SVGCAIRO}              "
echo "* gtkglext      : ${ENABLE_GTKGLEXT}              "
echo "* gconf         : ${ENABLE_GCONF}                 "
echo "* sourceview    : ${ENABLE_SOURCEVIEW}            "
echo "* mozembed      : ${ENABLE_MOZEMBED}              "
echo "* soegtk        : yes                             "
echo "* gnomevfs      : ${ENABLE_GNOMEVFS}              "
echo "* gstreamer     : ${ENABLE_GSTREAMER}             "
echo "* documentation : ${BUILDDOCS}                    "
dnl echo "* tutorials     : no                              "
echo "*                                                 "
echo "* Now do \"(g)make\" followed by \"(g)make install\""
echo "**************************************************"
