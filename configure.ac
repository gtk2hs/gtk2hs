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

AC_INIT([gtk2hs],[0.9.10.2])
AM_INIT_AUTOMAKE

dnl * We require autoconf version 2.59
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
AC_PATH_PROG(BASENAME,basename)
AC_PATH_PROG(GREP,gnugrep)
AC_PATH_PROG(GREP,ggrep)
AC_PATH_PROG(GREP,grep)
AC_PATH_PROG(GZIP,gzip)
AC_PATH_PROG(SED,gnused)
AC_PATH_PROG(SED,gsed)
AC_PATH_PROG(SED,sed)
AC_PATH_PROG(CUT,cut)
AC_PATH_PROG(TAR,tar)
AC_PATH_PROG(TOUCH,touch)
AC_PROG_RANLIB

dnl Figure out file extensions.
AC_EXEEXT

dnl Check for library pre- and suffixes.
if test x$EXEEXT = x; then
  dnl must be unix
  HSCFLAGS=;
  C2HSFLAGS=;
  dnl Where are we? (only used during configuration)
  CREATE_TYPES="plugNsocket default";
  STARTUP_CODE=;
else
  dnl must be Win32
  HSCFLAGS="-C -optc-mms-bitfields"
  C2HSFLAGS=;
  dnl Where are we? (only used during configuration)
  WIN32=yes;
  CREATE_TYPES=default;
  # It would be nice to say -Wl,--subsystem,windows which seems to be the
  # normal way. However, Solaris' sed is broken and won't deal with the
  # commas correctly: Symptom: "./configure: Bad substitution"
  
  # For the moment do not use the --subsystem windows flag since if you write
  # to stdout the program will throw an IO exception and die without any
  # explanation. Apparently ghc 6.4 will make this better in that it will
  # display the exception so it will not be so confusing.
  #STARTUP_CODE="-mwindows";
  STARTUP_CODE=;
fi;

AM_CONDITIONAL(WIN32, test "$WIN32" = "yes")

dnl Check for libraries.

dnl Is Manuels Ports library present?
dnl AC_ARG_WITH(ports, 
dnl	    [  --with-ports=PORTS-CONFIG use this Haskell Ports Library],
dnl	    [PORTS_CONFIG=$withval])


dnl Select a specific Haskell compiler and/or flags.
AC_ARG_WITH(hc, 
            [  --with-hc=HC            use Haskell compiler HC],
	    [HC=$withval])

AC_ARG_WITH(hcflags, 
            [  --with-hcflags=HCFLAGS  flags for Haskell tools, default: -O],
	    [HCFLAGS=$withval],[HCFLAGS=-O])



dnl Check for GHC-5.04 or greater.
dnl (The next command is not executed, if $HC is already set.)
AC_PATH_PROG(HC, [$HC ghc], ghc-not-found)
if test $HC = ghc-not-found; then
 AC_MSG_ERROR([
Could not find GHC!  This is the only supported compiler.
You need GHC 5.04 upwards.])
fi

GHC=$HC

dnl Check GHC details.
AC_MSG_CHECKING([version of GHC])
GHC_VERSION=`$GHC --numeric-version`
AC_MSG_RESULT([$GHC_VERSION])

GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -lt, 5.0.4, [
  AC_MSG_ERROR([I need the FFI of GHC 5.04 upwards!])])

dnl Calculate a version number with 3 digits (i.e. 502 for 5.2)
GHC_VERSION_NUMBER=`echo $GHC_VERSION | $SED "s/[[0-9]*\.\([0-9]]*\).*/0\1/"`
GHC_VERSION_NUMBER=`echo $GHC_VERSION_NUMBER | $SED "s/[[0-9]*\([0-9][0-9]]\)/\1/"`
GHC_VERSION_NUMBER=`echo $GHC_VERSION | $SED "s/\([[0-9]]\).*/\1/"`$GHC_VERSION_NUMBER

AC_DEFINE_UNQUOTED(__GLASGOW_HASKELL__, $GHC_VERSION_NUMBER,
	[Version number of GHC.])

dnl From ghc 6 onwards, ghc-pkg has the "auto libs" feature which means
dnl users do not have to specify the -package flag most of the time.
dnl All our packages support this mode now.
dnl Earlieir version of ghc-pkg choke on this package configuration option,
dnl hence this test which only enables it for ghc 6 onwards.
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -ge, 6.0.0, [
GHCPKG_USE_AUTOLIBS="auto            = True,"
])

dnl From ghc 6.4 onwards, ghc-pkg uses a new package format so we must produce
dnl and register .package.conf files instead of the old .pkg files.
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -ge, 6.4.0, [
USE_NEW_PKG_FORMAT=yes
])
AM_CONDITIONAL(USE_NEW_PKG_FORMAT, test "$USE_NEW_PKG_FORMAT" = "yes")
if test "$USE_NEW_PKG_FORMAT" = "yes"; then
  PKGEXT=package.conf
else
  PKGEXT=pkg
fi

dnl ghc-pkg 5 searches for libxxx.a, even on Windows. Hence, we need to
dnl create libraries with these names instead of the windows names.
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -lt, 6.0.0, [
  SLSUFFIX=".a";
  SLPREFIX="lib";
  DLSUFFIX=".so";
  DLPREFIX="lib";
])

GHCBARE=`basename $GHC`
GHCDIR=`dirname $GHC`
GHCPKGNAME=ghc-pkg`echo $GHCBARE | $SED s/ghc//`

dnl Check for ghc-pkg. Use the one that is in the same directory and
dnl version suffix as the specified compiler.
AC_PATH_PROGS(GHCPKG, $GHCPKGNAME ghc-pkg, ghcpkg-not-found, $GHCDIR)

if test $GHCPKG = ghcpkg-not-found; then
  AC_MSG_ERROR([ghc-pkg not found. (But ghc exists!?)]);
fi

dnl On Solaris ghc-pkg fails to build GHCi .o files when installing because
dnl it uses a linker command that is only appropriate for GNU ld, not the
dnl Solaris native ld. Disable building GHCi .o files on Solaris until this
dnl is fixed. (And then make it conditional on the GHC version, ho hum)
case "$host_os" in
  solaris2*) GHCPKG_BUILD_GHCI_LIB=;;
          *) GHCPKG_BUILD_GHCI_LIB=--auto-ghci-libs;;
esac

dnl Define WIN32 on win32 platform. Perhaps this should go in the
dnl gtk2hs-config.h file?
if test "$WIN32" = "yes"; then
  HCFLAGS="$HCFLAGS -DWIN32"
fi

AC_ARG_ENABLE(split-objs,
	     [  --enable-split-objs     produce smaller executables (on supported platforms)],
	     [ENABLE_SPLITOBJS=$enableval],[ENABLE_SPLITOBJS=no])
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
AC_ARG_WITH(pkgconf,
	    [  --with-pkgconf=FILE     GHC package file to install packages],
	    [PKGCONF=$withval;],[PKGCONF=;])

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
PKG_CHECK_MODULES(GTK,[pango >= 1.0.0 gdk-2.0 >= 2.0.0 gdk-pixbuf-2.0 >= 0.12.0 gtk+-2.0 >= 2.0.0], [], AC_MSG_ERROR([

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

dnl Use in this order of preference: seamonkey, firefox, xulrunner, mozilla
dnl If we have xulrunner, use it instead of firefox or mozilla
if test "$ENABLE_SEAMONKEY_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$SEAMONKEY_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$SEAMONKEY_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$SEAMONKEY_MOZEMBED_LIBDIR"
elif test "$ENABLE_FIREFOX_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$FIREFOX_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$FIREFOX_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$FIREFOX_MOZEMBED_LIBDIR"
elif test "$ENABLE_XULRUNNER_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$XULRUNNER_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$XULRUNNER_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$XULRUNNER_MOZEMBED_LIBDIR"
elif test "$ENABLE_MOZILLA_MOZEMBED" = "yes"; then
	ENABLE_MOZEMBED="yes"
	MOZEMBED_CFLAGS="$MOZILLA_MOZEMBED_CFLAGS"
	MOZEMBED_LIBS="$MOZILLA_MOZEMBED_LIBS"
	MOZEMBED_LIBDIR="$MOZILLA_MOZEMBED_LIBDIR"
fi
AC_DEFINE_UNQUOTED(MOZEMBED_LIBDIR,"$MOZEMBED_LIBDIR",[The mozembed library directory])
AM_CONDITIONAL(ENABLE_MOZEMBED, test "$ENABLE_MOZEMBED" = "yes")
AC_SUBST(MOZEMBED_CFLAGS)
AC_SUBST(MOZEMBED_LIBS)

GTKHS_PKG_CHECK(cairo, cairo, CAIRO, [cairo >= 1.0.0],
	[build bindings for the cairo vector graphics system (default=auto)],
	[cairo requirement not met. Perhaps you need to install cairo or cairo-devel])

GTKHS_PKG_CHECK(svg-cairo, svgcairo, SVGCAIRO, [libsvg-cairo >= 0.1.4],
	[build bindings for the cairo svg extension (default=auto)],
	[libsvg-cairo requirement not met. Perhaps you need to install libsvg-cairo or libsvg-cairo-devel])

GTKHS_PKG_CHECK(opengl, gtkglext, GTKGLEXT, [gtkglext-1.0 >= 1.0.5],
 	[build bindings for the Gtk OpenGL extension (default=auto)],
 	[GtkGLExt library requirement not met. Perhaps you need to install gtkglext or gtkglext-devel])


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

AM_CONDITIONAL(HAVE_GTK_VERSION_2_2, test "$HAVE_GTK_VERSION_2_2" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_4, test "$HAVE_GTK_VERSION_2_4" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_6, test "$HAVE_GTK_VERSION_2_6" = "yes")
AM_CONDITIONAL(HAVE_GTK_VERSION_2_8, test "$HAVE_GTK_VERSION_2_8" = "yes")

CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_2" = "yes" && echo gtk-2.2`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_4" = "yes" && echo gtk-2.4`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_6" = "yes" && echo gtk-2.6`"
CREATE_TYPES="$CREATE_TYPES `test "$HAVE_GTK_VERSION_2_8" = "yes" && echo gtk-2.8`"

dnl Also allow us to conditionally compile binding to the new Gtk+ APIs.
GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`
GTK_MAJOR_VERSION=`echo $GTK_VERSION | $CUT -d. -f1`
GTK_MINOR_VERSION=`echo $GTK_VERSION | $CUT -d. -f2`
GTK_MICRO_VERSION=`echo $GTK_VERSION | $CUT -d. -f3`
AC_DEFINE_UNQUOTED(GTK_MAJOR_VERSION, ($GTK_MAJOR_VERSION), [Gtk major version number])
AC_DEFINE_UNQUOTED(GTK_MINOR_VERSION, ($GTK_MINOR_VERSION), [Gtk minor version number])
AC_DEFINE_UNQUOTED(GTK_MICRO_VERSION, ($GTK_MICRO_VERSION), [Gtk minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of GTK+ */
#define GTK_CHECK_VERSION(major,minor,micro)    \
    (GTK_MAJOR_VERSION > (major) || \
     (GTK_MAJOR_VERSION == (major) && GTK_MINOR_VERSION > (minor)) || \
     (GTK_MAJOR_VERSION == (major) && GTK_MINOR_VERSION == (minor) && \
      GTK_MICRO_VERSION >= (micro)))
])

dnl Also allow us to conditionally compile bindings to Pango APIs.
PANGO_VERSION=`$PKG_CONFIG pango --modversion`
PANGO_MAJOR_VERSION=`echo $PANGO_VERSION | $CUT -d. -f1`
PANGO_MINOR_VERSION=`echo $PANGO_VERSION | $CUT -d. -f2`
PANGO_MICRO_VERSION=`echo $PANGO_VERSION | $CUT -d. -f3`
AC_DEFINE_UNQUOTED(PANGO_MAJOR_VERSION, ($PANGO_MAJOR_VERSION), [Pango major version number])
AC_DEFINE_UNQUOTED(PANGO_MINOR_VERSION, ($PANGO_MINOR_VERSION), [Pango minor version number])
AC_DEFINE_UNQUOTED(PANGO_MICRO_VERSION, ($PANGO_MICRO_VERSION), [Pango minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of Pango */
#define PANGO_CHECK_VERSION(major,minor,micro)    \
    (PANGO_MAJOR_VERSION > (major) || \
     (PANGO_MAJOR_VERSION == (major) && PANGO_MINOR_VERSION > (minor)) || \
     (PANGO_MAJOR_VERSION == (major) && PANGO_MINOR_VERSION == (minor) && \
      PANGO_MICRO_VERSION >= (micro)))
])

dnl Also allow us to conditionally compile bindings to Glib APIs.
GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`
GLIB_MAJOR_VERSION=`echo $GLIB_VERSION | $CUT -d. -f1`
GLIB_MINOR_VERSION=`echo $GLIB_VERSION | $CUT -d. -f2`
GLIB_MICRO_VERSION=`echo $GLIB_VERSION | $CUT -d. -f3`
AC_DEFINE_UNQUOTED(GLIB_MAJOR_VERSION, ($GLIB_MAJOR_VERSION), [Glib major version number])
AC_DEFINE_UNQUOTED(GLIB_MINOR_VERSION, ($GLIB_MINOR_VERSION), [Glib minor version number])
AC_DEFINE_UNQUOTED(GLIB_MICRO_VERSION, ($GLIB_MICRO_VERSION), [Glib minor patch level])
AH_BOTTOM([
/* Allow code to be compiled differently for different versions of Glib */
#define GLIB_CHECK_VERSION(major,minor,micro)    \
    (GLIB_MAJOR_VERSION > (major) || \
     (GLIB_MAJOR_VERSION == (major) && GLIB_MINOR_VERSION > (minor)) || \
     (GLIB_MAJOR_VERSION == (major) && GLIB_MINOR_VERSION == (minor) && \
      GLIB_MICRO_VERSION >= (micro)))
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

dnl c2hs Dilemma.
dnl Check if the user supplied a specific C->Haskell installation or wants to
dnl use the version in the current search path (--with-c2hs-config=yes). 
dnl The default is to use the built-in version.
AC_ARG_WITH(c2hs, 
  [  --with-c2hs=C2HS        use an external C->Haskell installation (slower!)],
  [
    case $withval in
      yes) {
	BUILT_IN_C2HS=no;
        AC_PATH_PROG(C2HS, c2hs, notfound)
        if test $C2HS = notfound; then
          AC_MSG_ERROR([C->Haskell was not found in current search path.
	          Try compiling with the built-in c2hs by omitting
		  --with-c2hs=... when calling ./configure .])
	fi
	} ;;
      no) {
	BUILT_IN_C2HS=yes
	} ;;
      *) {
	BUILT_IN_C2HS=no;
	AC_PATH_PROG(C2HS, $withval, notfound)
        if test $C2HS = notfound; then
	  AC_MSG_ERROR([The specified C->Haskell tool was not found.
	          Try compiling with the built-in c2hs by omitting
		  --with-c2hs=... when calling ./configure .])
	fi
	} ;;
    esac
  ],[BUILT_IN_C2HS=yes])

dnl The big switch differing between built-in and external c2hs.

AC_MSG_CHECKING([kind of C->Haskell])
if test $BUILT_IN_C2HS = yes; then
  AC_MSG_RESULT([built-in])
  dnl Use the local c2hs.
  C2HS='./tools/c2hs/c2hsLocal$(EXEEXT)';

else
  AC_MSG_ERROR([Using an external c2hs is not supported in this release,
		though hopefully this feature will return in the next release.
		Please try compiling with the built-in c2hs by omitting
		--with-c2hs=... when calling ./configure .])
  AC_MSG_RESULT([external])
  case $C2HS in
    c2hs-gtk2hs) {
      MULTIPLE_CHS=yes;
      } ;;
    *) {
      MULTIPLE_CHS=no;
      dnl Find C->Haskell and check its version.
      dnl Check the version of c2hs
      AC_CACHE_CHECK([c2hs version], c2hs_version, [
        c2hs_version=`$C2HS --version | $SED "s/[[^0-9.]*\([0-9.]*\) .*]/\1/"`;
      ])
      GTKHS_PROG_CHECK_VERSION($c2hs_version, -lt, 0.13.4,
        AC_MSG_ERROR([You need C->Haskell version 0.13.4 upwards!
          ** Download from \"http://www.cse.unsw.edu.au/~chak/haskell/c2hs/\". **]))
      dnl C->Haskell configuration.
      } ;;
  esac;
fi

dnl The hsc2hs preprocessor
AC_PATH_PROG(HSC2HS,hsc2hs, no)
if test "$HSC2HS" = "no"; then
  AC_MSG_ERROR([Could not find hsc2hs. This should be installed with GHC!])
fi

dnl The happy parser generator
if test "$BUILT_IN_C2HS" = yes; then
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
fi

dnl The alex lexer generator
if test "$BUILT_IN_C2HS" = yes; then
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
fi
AM_CONDITIONAL(BUILDDOCS, test "$BUILDDOCS" = "yes")

AC_ARG_WITH(ghc-docdir, 
            [  --with-ghc-docdir=GHC_DOCDIR  location of top of ghc haddockified html documentation],
	    [GHC_DOCDIR=$withval])

dnl The preprocessor to use on Haskell sources
if test "$USE_CPP_LINES" = "yes"; then
HSCPP='$(CPP) -x c -traditional-cpp'
else
HSCPP='$(CPP) -x c -traditional-cpp -P'
fi

dnl Needed substitution.
AC_SUBST(BUILT_IN_C2HS)
AC_SUBST(MULTIPLE_CHS)
AC_SUBST(PKGCONF)
AC_SUBST(USE_NEW_PKG_FORMAT)
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
AC_SUBST(GHCPKG_USE_AUTOLIBS)
AC_SUBST(GHCPKG_BUILD_GHCI_LIB)
dnl Optional packages
AC_SUBST(GTK_CAIRO_DEPEND)
AC_SUBST(GTK_CAIRO_DEPEND_NO_CABAL)
dnl Documentation
AC_SUBST(HADDOCK)
AC_SUBST(GHC_DOCDIR)
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
  ])
AC_CONFIG_COMMANDS([default],[[chmod a+x mk/chsDepend mk/link-splitobjs.sh install-sh]])
AC_OUTPUT

dnl ...and chat with the user
echo "**************************************************"
echo "* Configuration completed successfully.          *"
echo "*                                                *"
dnl if test x$BUILDDOCS = xyes && test -z "$HADDOCK"; then
dnl 	echo "* Warning: The documentation will not be built:  *"
dnl 	echo "*   - haddock was not found                      *"
dnl 	echo "*                                                *"
dnl fi
dnl if test $BUILDDOCS = no; then
dnl   echo "* Warning: The documentation will not be built:  *"
dnl   if test $FOUNDTRANSLATOR = no; then
dnl     echo "*   - the xsltproc translator was not found      *"
dnl   fi;
dnl   if test $FOUNDCATALOG = no; then
dnl     echo "*   - no XML catalog files were found            *"
dnl   fi;
dnl   if test $FOUNDHTML = no; then
dnl     echo "*   - no HTML XSL translation file was found     *"
dnl   fi;
dnl   if test $FOUNDFO = no; then
dnl     echo "*   - no FO XSL translation file was found       *"
dnl   fi;
dnl   echo "*                                                *"
dnl fi;
dnl if test $ENABLE_OPENGL = no; then
dnl  echo "* Warning: OpenGL support is not available:      *"
dnl  if test x$FOUNDGLEXT = xno; then
dnl    echo "*   - the GtkGLExt widget is not installed       *"
dnl  fi;
dnl  if test x$FOUNDHOPENGL = xno; then
dnl    echo "*   - HOpenGL is not installed in the specified  *"
dnl    echo "*     GHC installation                           *"
dnl  fi;
dnl  echo "*                                                *"
dnl fi;
echo "* Now do \"(g)make\" followed by \"(g)make install\" *"
echo "**************************************************"
