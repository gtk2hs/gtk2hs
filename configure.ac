dnl Mogul - a monad based gui library
dnl
dnl Copyright (c) 2001, 2002 Axel Simon <as49@ukc.ac.uk>
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

AC_INIT(gtk2hs, 0.9.7pre)
AM_INIT_AUTOMAKE

dnl * We require autoconf version 2.50
dnl We need 2.50 due to the use of OBJEXT 
AC_PREREQ(2.50)

dnl Check system type.
AC_CANONICAL_HOST

AM_CONFIG_HEADER(config.h)
AH_TOP([
/* Hack to suppress warnings that these symbols clash with
   the ones from ghc's version of config.h */
#undef /**/ PACKAGE_NAME
#undef /**/ PACKAGE_STRING
#undef /**/ PACKAGE_TARNAME
#undef /**/ PACKAGE_VERSION
])

dnl Checks for programs.
AM_PROG_CC_C_O
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

dnl On some systems (Solaris,...) asking sh for the current directory
dnl (with a built-in pwd) and asking make for the current directory
dnl gives different results. Thus, if possible, we use the same external
dnl program for determining the current path.
dnl The AC_PATH_PROG macro does not work because pwd is a built-in and
dnl therefore evaluated in the body of the expanded macro.
AC_MSG_CHECKING(for external pwd)
PWD=`which pwd 2> /dev/null`
if test -z "$PWD"; then
  AC_MSG_RESULT([none found])
  PWD="pwd";
else
  AC_MSG_RESULT($PWD)
fi;

dnl Figure out file extensions.
AC_EXEEXT

dnl Check for library pre- and suffixes.
if test x$EXEEXT = x; then
  dnl must be unix
  SLSUFFIX=".a";
  SLPREFIX="lib";
  DLSUFFIX=".so";
  DLPREFIX="lib";
  dnl the hash sign is a comment for sed
  PATHSED=["\#"];
  HSCFLAGS=;
  C2HSFLAGS=" -C-D__signed=signed";
  dnl Where are we? (only used during configuration)
  TOP=`$PWD`;
  CREATE_TYPES="plugNsocket default";
else
  dnl must be Win32
  SLSUFFIX=".lib";
  SLPREFIX="";
  DLSUFFIX=".dll";
  DLPREFIX="";
  dnl replace something like /c/ with c:/
  case $host_os in
    *cygwin* ) PATHSED=["s+/cygdrive/\([a-z]\)/+\1:/+"];;
    *mingw32* ) PATHSED=["s+/\([a-z]\)/+\1:/+"];;
    * ) PATHSED=["\#"];; 
  esac;
  HSCFLAGS=" -C -optc-fnative-struct";
  C2HSFLAGS=;
  dnl Where are we? (only used during configuration)
  TOP=`$PWD | $SED $PATHSED`;
  WIN32=yes;
  CREATE_TYPES=default;
fi;

if test "$WIN32" = "yes"; then
  AC_DEFINE(WIN32, [], [Are we building on a Win32 system])
fi

dnl determine a temporary directory for c2hs
TMPDIR="/tmp"
if test "$WIN32" = "yes"; then
  TMPDIR="C:\TMP"
fi
AC_SUBST(TMPDIR)

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
            [  --with-hcflags=HCFLAGS  flags for Haskell tools, default: -O -H180m],
	    [HCFLAGS=$withval])



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
GHC_VERSION=`$GHC --version | $SED "s/[[a-zA-Z ,]*\([0-9.]*\)[a-zA-Z ]]*/\1/"`
AC_MSG_RESULT([$GHC_VERSION])

GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -lt, 5.0.4, [
  AC_MSG_ERROR([I need the FFI of GHC 5.04 upwards!])])

dnl Calculate a version number with 3 digits (i.e. 502 for 5.2)
GHC_VERSION_NUMBER=`echo $GHC_VERSION | $SED "s/[[0-9]*\.\([0-9]]*\).*/0\1/"`
GHC_VERSION_NUMBER=`echo $GHC_VERSION_NUMBER | $SED "s/[[0-9]*\([0-9][0-9]]\)/\1/"`
GHC_VERSION_NUMBER=`echo $GHC_VERSION | $SED "s/\([[0-9]]\).*/\1/"`$GHC_VERSION_NUMBER

__GLASGOW_HASKELL__=$GHC_VERSION_NUMBER
AC_DEFINE_UNQUOTED(__GLASGOW_HASKELL__, $GHC_VERSION_NUMBER,
	[Version number of GHC.])

dnl Check if the ghc compiler can generate dynamic callbacks with more than
dnl 4 words worth of arguments. Hopefully one day the compiler will support
dnl this.
AC_MSG_CHECKING([broken dynamic callbacks])
FOUR_WORD_CALLBACK=no
GTKHS_PROG_CHECK_VERSION($GHC_VERSION, -lt, 9.9.9, [
  if test $host_cpu = sparc; then FOUR_WORD_CALLBACK=yes; fi
  dnl TODO: is this only on Sparc Solaris or on all Sparc (ie Linux too)?
])
AC_MSG_RESULT([$FOUR_WORD_CALLBACK])

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

dnl Optimise Haskell by default and give sufficient space.
if test -z "$HCFLAGS"; then
  HCFLAGS="-O -H180m"
fi

dnl Check whether to use a local package file.
AC_ARG_WITH(pkgconf,
	    [  --with-pkgconf=FILE     GHC package file to install packages],
	    [PKGCONF=$withval;],[PKGCONF=;])

dnl Check for pkg-config which holds information about all Gtk related
dnl libraries.
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

if test "$PKG_CONFIG" = "no" ; then
 AC_MSG_ERROR([*** The pkg-config script could not be found. Make sure it is
   *** in your path, or set the PKG_CONFIG environment variable
   *** to the full path to pkg-config.
   *** Or see http://www.freedesktop.org/software/pkgconfig to get pkg-config.
  ])
fi;


dnl dnl Check if user wants GtkGLExt extension. Make the default value for this
dnl dnl flag dependant on whether it is installed or not.
dnl ENABLE_OPENGL=yes;
dnl AC_MSG_CHECKING([if OpenGL extension can be built])
dnl 
dnl AC_ARG_ENABLE(glext,
dnl 	      [  --enable-glext          generate binding for OpenGL rendering],
dnl 	      [FOUNDGLEXT=$enableval],[FOUNDGLEXT=yes])
dnl 
dnl dnl Reset the flag if GtkGLExt is not installed
dnl $PKG_CONFIG --exists gtkglext-1.0 || FOUNDGLEXT=no;
dnl if test x$FOUNDGLEXT = xno; then ENABLE_OPENGL=no;fi;
dnl 
dnl dnl Reset the flag if HOpenGL is not installed
dnl FOUNDHOPENGL=yes;
dnl $GHCPKG -l | $GREP OpenGL > /dev/null || FOUNDHOPENGL=no;
dnl if test x$FOUNDHOPENGL = xno; then ENABLE_OPENGL=no;fi;
dnl 
dnl AC_MSG_RESULT($ENABLE_OPENGL)

dnl Check if user wants libglade bindings. Defaults to yes.
AC_MSG_CHECKING([whether to build libglade bindings])

AC_ARG_ENABLE(libglade,
              [  --disable-libglade      do not generate libglade bindings],
              [ENABLE_LIBGLADE=$enableval],[ENABLE_LIBGLADE=yes])

AC_MSG_RESULT($ENABLE_LIBGLADE)
AM_CONDITIONAL(ENABLE_LIBGLADE, test x$ENABLE_LIBGLADE = xyes)

dnl Check if user wants the various gnome modules. Defaults to yes.
AC_MSG_CHECKING([whether to build gnome bindings])

AC_ARG_ENABLE(gnome,
              [  --disable-gnome         do not generate bindings for any gnome modules],
              [ENABLE_GNOME=$enableval],[ENABLE_GNOME=yes])

AC_MSG_RESULT($ENABLE_GNOME)
AM_CONDITIONAL(ENABLE_GNOME, test x$ENABLE_GNOME = xyes)

dnl Check if user wants the Mozilla's browser engine widget. Defaults to yes.
AC_MSG_CHECKING([whether to build mozembed bindings])

AC_ARG_ENABLE(mozilla,
              [  --disable-mozilla       do not generate bindings for the Mozilla display widget],
              [ENABLE_MOZEMBED=$enableval],[ENABLE_MOZEMBED=yes])

AC_MSG_RESULT($ENABLE_MOZEMBED)
AM_CONDITIONAL(ENABLE_MOZEMBED, test x$ENABLE_MOZEMBED = xyes)

dnl Check for the GTK&Co libraries. Use the special PKG_CHECK_MODULES
dnl macro from the pkg-config program.
PKG_CHECK_MODULES(GLIB,[glib-2.0 >= 2.0.0])
PKG_CHECK_MODULES(GTK,[gdk-2.0 >= 2.0.0 gtk+-2.0 >= 2.0.0 gdk-pixbuf-2.0 >= 0.12.0])
dnl if test x$ENABLE_OPENGL = xyes; then
dnl   PKG_CHECK_MODULES(GTKGLEXT,[gtkglext-1.0 >= 0.7.1])
dnl fi
if test x$ENABLE_LIBGLADE = xyes; then
  PKG_CHECK_MODULES(LIBGLADE,[libglade-2.0 >= 2.0.0])
fi
if test x$ENABLE_GNOME = xyes; then
  PKG_CHECK_MODULES(GCONF,[gconf-2.0 >= 2.0.0])
  PKG_CHECK_MODULES(SOURCEVIEW,[gtksourceview-1.0 >= 0.6.0])
fi
if test x$ENABLE_MOZEMBED = xyes; then
  PKG_CHECK_MODULES(MOZEMBED,[mozilla-gtkmozembed >= 1.4])
fi


dnl Some APIs only appeared in later versions of libraries. Generate only
dnl Haskell types for the available C types.
CREATE_TYPES="$CREATE_TYPES `$PKG_CONFIG gtk+-2.0 --atleast-version=2.2 && echo gtk-2.2`"
CREATE_TYPES="$CREATE_TYPES `$PKG_CONFIG gtk+-2.0 --atleast-version=2.4 && echo gtk-2.4`"
CREATE_TYPES="$CREATE_TYPES `$PKG_CONFIG gtk+-2.0 --atleast-version=2.6 && echo gtk-2.6`"

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

dnl The configuration program for GTK is kind of stupid in that it
dnl lists directories which don't exist. ghc-pkg in ghc 5.04 or greater
dnl does not like that, so we need to filter out non-existent directories.
dnl Furthermore we remove all libraries and directories for packages that
dnl built on top of gtk so that they don't appear several times on the
dnl command line.
GLIB_CFLAGS=`tools/checkDirs.sh $GLIB_CFLAGS`;
GLIB_LIBS=`tools/checkDirs.sh $GLIB_LIBS`;

GTK_CFLAGS=`CFLAGS="$GLIB_CFLAGS" tools/checkDirs.sh $GTK_CFLAGS`;
GTK_LIBS=`LDFLAGS="$GLIB_LIBS" tools/checkDirs.sh $GTK_LIBS`;

SOURCEVIEW_CFLAGS=`CFLAGS="$GLIB_CFLAGS $GTK_CFLAGS" tools/checkDirs.sh $SOURCEVIEW_CFLAGS`;
SOURCEVIEW_LIBS=`LDFLAGS="$GLIB_LIBS $GTK_LIBS" tools/checkDirs.sh $SOURCEVIEW_LIBS`;

LIBGLADE_CFLAGS=`CFLAGS="$GLIB_CFLAGS $GTK_CFLAGS" tools/checkDirs.sh $LIBGLADE_CFLAGS`;
LIBGLADE_LIBS=`LDFLAGS="$GLIB_LIBS $GTK_LIBS" tools/checkDirs.sh $LIBGLADE_LIBS`;

GCONF_CFLAGS=`CFLAGS="$GLIB_CFLAGS $GTK_CFLAGS" tools/checkDirs.sh $GCONF_CFLAGS`;
GCONF_LIBS=`LDFLAGS="$GLIB_LIBS $GTK_LIBS" tools/checkDirs.sh $GCONF_LIBS`;

MOZEMBED_CFLAGS=`CFLAGS="$GLIB_CFLAGS $GTK_CFLAGS" tools/checkDirs.sh $MOZEMBED_CFLAGS`;
MOZEMBED_LIBS=`LDFLAGS="$GLIB_LIBS $GTK_LIBS" tools/checkDirs.sh $MOZEMBED_LIBS`;


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

AC_ARG_WITH(hidir,
	[  --with-hidir=DIR        specify install dir for .hi files],
	[hidir=$withval],
	[hidir='${pkglibdir}/imports'])
AC_SUBST(hidir)

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

dnl Have a special marshall list (available in the source tree of Gtk+ under
dnl gtk/gtkmarshal.list)

AC_MSG_CHECKING(marshal list)
AC_ARG_WITH(mlist, 
	    [  --with-mlist=GTK-SOURCE use special marshall list from GTK+ sources],
	    [MARSHALLDEFS=$withval;
	     AC_MSG_RESULT($withval)],
	    [MARSHALLDEFS='$(TOP)/tools/callbackGen/gtkmarshal.list';
	     AC_MSG_RESULT(built-in)])



dnl c2hs Dilemma.
dnl We provide a local copy of Manuel's c2hs which is patched so that is can
dnl process several .chs files at the same invocation. This patch was not
dnl incooperated into the main c2hs version since Manuel wants to do some
dnl other fundamental changes first. The nice thing is that with the built-in
dnl c2hs this whole package can be built without any other libraries.

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
  C2HS='$(TOP)/tools/c2hs/c2hsLocal';

  dnl These are the settings needed to compile c2hs.
  LEGACY_FFI=no;
  BEGIN_LEGACY_FFI="{- for systems including the Legacy FFI";
  END_LEGACY_FFI="-}";
  BEGIN_NEW_FFI=;
  END_NEW_FFI=;
  BEGIN_NHC="{- NHC does some things differently...";
  END_NHC="-}";
  BEGIN_NOT_NHC=;
  END_NOT_NHC=;

else
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
AC_PATH_PROG(HSC2HS,hsc2hs)
if test "$HSC2HS" = "no"; then
  AC_MSG_ERROR([Could not find hsc2hs. This should be isntalled with GHC!])
fi

dnl The preprocessor to use on Haskell sources
HSCPP='$(CPP) -x c -traditional-cpp -P'

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
  GTKHS_PROG_CHECK_VERSION($HADDOCK_VERSION, -lt, 0.6, [
    AC_MSG_ERROR([I need haddock 0.6 or later to build the documentation.
    Upgrade your version of haddock or re-run configure without --enable-docs])])
fi
AM_CONDITIONAL(BUILDDOCS, test "$BUILDDOCS" = "yes")

AC_ARG_WITH(ghc-docdir, 
            [  --with-ghc-docdir=GHC_DOCDIR  location of top of ghc haddockified html documentation],
	    [GHC_DOCDIR=$withval])

dnl Needed substitution.
AC_SUBST(PWD)
AC_SUBST(TOP)
AC_SUBST(BUILT_IN_C2HS)
AC_SUBST(MULTIPLE_CHS)
AC_SUBST(FOUR_WORD_CALLBACK)
AC_SUBST(GHCPKG_LISTLOCAL)
AC_SUBST(PKGCONF)
AC_SUBST(HCFLAGS)
AC_SUBST(C2HS)
AC_SUBST(HSC2HS)
AC_SUBST(HSCPP)
AC_SUBST(MARSHALLDEFS)
AC_SUBST(VERSION)
dnl Platform specific flags 
AC_SUBST(DLPREFIX)
AC_SUBST(DLSUFFIX)
AC_SUBST(SLPREFIX)
AC_SUBST(SLSUFFIX)
AC_SUBST(WIN32)
AC_SUBST(PATHSED)
AC_SUBST(HSCFLAGS)
AC_SUBST(C2HSFLAGS)
AC_SUBST(EXTRA_HFILES)
dnl Versionitis
AC_SUBST(CREATE_TYPES)
dnl Optional packages
AC_SUBST(SOURCEVIEW_CFLAGS)
AC_SUBST(SOURCEVIEW_LIBS)
AC_SUBST(LIBGLADE_CFLAGS)
AC_SUBST(LIBGLADE_LIBS)
AC_SUBST(GCONF_CFLAGS)
AC_SUBST(GCONF_LIBS)
dnl Documentation
AC_SUBST(HADDOCK)
AC_SUBST(GHC_DOCDIR)
dnl The c2hs part.
AC_SUBST(CPP)
AC_SUBST(LEGACY_FFI)
AC_SUBST(BEGIN_LEGACY_FFI)
AC_SUBST(END_LEGACY_FFI)
AC_SUBST(BEGIN_NEW_FFI)
AC_SUBST(END_NEW_FFI)
AC_SUBST(BEGIN_NHC)
AC_SUBST(END_NHC)
AC_SUBST(BEGIN_NOT_NHC)
AC_SUBST(END_NOT_NHC)

dnl write the results...
AC_CONFIG_FILES([
  Makefile
])

AC_OUTPUT([
  tools/c2hs/toplevel/C2HSConfig.hs
  gtk2hs.spec
  mk/chsDepend
  glib/glib.pkg
  gtk/gtk.pkg
  mogul/mogul.pkg
  glade/glade.pkg
  gconf/gconf.pkg
  sourceview/sourceview.pkg
  mozembed/mozembed.pkg],
  [chmod a+x mk/chsDepend && chmod a+x install-sh])

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
