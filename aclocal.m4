# generated automatically by aclocal 1.7.5 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
# Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

dnl Gtk+HS - Additional macros for `autoconf'

dnl -- Pinched from FPTOOLS/GHC
dnl
dnl GTKHS_GHC_VERSION(version)
dnl GTKHS_GHC_VERSION(major, minor [, patchlevel])
dnl GTKHS_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
dnl NB: Don't use `+' in sed regexps; Jonas Svensson reports problems with it
dnl     on Solaris 8.
dnl
AC_DEFUN(GTKHS_GHC_VERSION,
[define([GTKHS_CV_GHC_VERSION], [gtkhs_cv_ghc_version])dnl
AC_CACHE_CHECK([version of ghc], GTKHS_CV_GHC_VERSION, [dnl
${GHC-ghc} --version > conftestghc 2>&1
  cat conftestghc >&AC_FD_CC
dnl `Useless Use Of cat' award...
changequote(<<, >>)dnl
  GTKHS_CV_GHC_VERSION=`cat conftestghc | sed -n -e 's/, patchlevel *\([0-9]\)/.\1/;s/.* version \([0-9.][0-9.]*\).*/\1/p'`
changequote([, ])dnl
  rm -fr conftest*
  if test "[$]GTKHS_CV_GHC_VERSION" = ""
  then
    GTKHS_CV_GHC_VERSION='unknown'
  fi])
changequote(<<, >>)dnl
GTKHS_CV_GHC_VERSION<<_major>>=`echo <<$>>GTKHS_CV_GHC_VERSION | sed -e 's/^\([0-9]\).*/\1/'`
GTKHS_CV_GHC_VERSION<<_minor>>=`echo <<$>>GTKHS_CV_GHC_VERSION | sed -e 's/^[0-9]\.\([0-9]*\).*/\1/'`
GTKHS_CV_GHC_VERSION<<_pl>>=`echo <<$>>GTKHS_CV_GHC_VERSION | sed -n -e 's/^[0-9]\.[0-9]*\.\([0-9]*\)/\1/p'`
changequote([, ])dnl
if test "[$]GTKHS_CV_GHC_VERSION[_pl]" = ""
then
  GTKHS_CV_GHC_VERSION[_all]="[$]GTKHS_CV_GHC_VERSION[_major].[$]GTKHS_CV_GHC_VERSION[_minor]"
  GTKHS_CV_GHC_VERSION[_pl]="0"
else
  GTKHS_CV_GHC_VERSION[_all]="[$]GTKHS_CV_GHC_VERSION[_major].[$]GTKHS_CV_GHC_VERSION[_minor].[$]GTKHS_CV_GHC_VERSION[_pl]"
fi
ifelse($#, [1], [dnl
[$1]="[$]GTKHS_CV_GHC_VERSION[_all]"
], $#, [2], [dnl
[$1]="[$]GTKHS_CV_GHC_VERSION[_major]"
[$2]="[$]GTKHS_CV_GHC_VERSION[_minor]"
], $#, [3], [dnl
[$1]="[$]GTKHS_CV_GHC_VERSION[_major]"
[$2]="[$]GTKHS_CV_GHC_VERSION[_minor]"
[$3]="[$]GTKHS_CV_GHC_VERSION[_pl]"
], $#, [4], [dnl
[$1]="[$]GTKHS_CV_GHC_VERSION[_all]"
[$2]="[$]GTKHS_CV_GHC_VERSION[_major]"
[$3]="[$]GTKHS_CV_GHC_VERSION[_minor]"
[$4]="[$]GTKHS_CV_GHC_VERSION[_pl]"
], [AC_MSG_ERROR([wrong number of arguments to [$0]])])dnl
undefine([GTKHS_CV_GHC_VERSION])dnl
])dnl

dnl -- Pinched from Michael Weber's HaskellMPI
dnl
dnl GTKHS_PROG_CHECK_VERSION(VERSIONSTR1, TEST, VERSIONSTR2,
dnl                          ACTION-IF-TRUE [, ACTION-IF-FALSE])
dnl
dnl compare versions field-wise (separator is '.')
dnl TEST is one of {-lt,-le,-eq,-ge,-gt}
dnl
dnl quite shell-independent and SUSv2 compliant code
dnl
dnl NOTE: the loop could be unrolled within autoconf, but the
dnl       macro code would be a) longer and b) harder to debug... ;)
dnl
AC_DEFUN(GTKHS_PROG_CHECK_VERSION,
[if ( IFS=".";
      a="[$1]";  b="[$3]";
      while test -n "$a$b"
      do
              set -- [$]a;  h1="[$]1";  shift 2>/dev/null;  a="[$]*"
              set -- [$]b;  h2="[$]1";  shift 2>/dev/null;  b="[$]*"
              test -n "[$]h1" || h1=0;  test -n "[$]h2" || h2=0
              test [$]{h1} -eq [$]{h2} || break
      done
      test [$]{h1} [$2] [$]{h2}
    )
then ifelse([$4],,[:],[
  $4])
ifelse([$5],,,
[else
  $5])
fi])])dnl





dnl PKG_CHECK_MODULES(GSTUFF, gtk+-2.0 >= 1.3 glib = 1.3.4, action-if, action-not)
dnl defines GSTUFF_LIBS, GSTUFF_CFLAGS, see pkg-config man page
dnl also defines GSTUFF_PKG_ERRORS on error
AC_DEFUN(PKG_CHECK_MODULES, [
  succeeded=no

  if test -z "$PKG_CONFIG"; then
    AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  fi

  if test "$PKG_CONFIG" = "no" ; then
     echo "*** The pkg-config script could not be found. Make sure it is"
     echo "*** in your path, or set the PKG_CONFIG environment variable"
     echo "*** to the full path to pkg-config."
     echo "*** Or see http://www.freedesktop.org/software/pkgconfig to get pkg-config."
  else
     PKG_CONFIG_MIN_VERSION=0.9.0
     if $PKG_CONFIG --atleast-pkgconfig-version $PKG_CONFIG_MIN_VERSION; then
        AC_MSG_CHECKING(for $2)

        if $PKG_CONFIG --exists "$2" ; then
            AC_MSG_RESULT(yes)
            succeeded=yes

            AC_MSG_CHECKING($1_CFLAGS)
            $1_CFLAGS=`$PKG_CONFIG --cflags "$2"`
            AC_MSG_RESULT($$1_CFLAGS)

            AC_MSG_CHECKING($1_LIBS)
            $1_LIBS=`$PKG_CONFIG --libs "$2"`
            AC_MSG_RESULT($$1_LIBS)
        else
            $1_CFLAGS=""
            $1_LIBS=""
            ## If we have a custom action on failure, don't print errors, but 
            ## do set a variable so people can do so.
            $1_PKG_ERRORS=`$PKG_CONFIG --errors-to-stdout --print-errors "$2"`
            ifelse([$4], ,echo $$1_PKG_ERRORS,)
        fi

        AC_SUBST($1_CFLAGS)
        AC_SUBST($1_LIBS)
     else
        echo "*** Your version of pkg-config is too old. You need version $PKG_CONFIG_MIN_VERSION or newer."
        echo "*** See http://www.freedesktop.org/software/pkgconfig"
     fi
  fi

  if test $succeeded = yes; then
     ifelse([$3], , :, [$3])
  else
     ifelse([$4], , AC_MSG_ERROR([Library requirements ($2) not met; consider adjusting the PKG_CONFIG_PATH environment variable if your libraries are in a nonstandard prefix so pkg-config can find them.]), [$4])
  fi
])



