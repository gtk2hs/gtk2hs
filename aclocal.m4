dnl aclocal.m4 generated automatically by aclocal 1.4-p5

dnl Copyright (C) 1994, 1995-8, 1999, 2001 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

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




# Configure paths for GTK+
# Owen Taylor     1997-2001

dnl AM_PATH_GTK_2_0([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GTK+, and define GTK_CFLAGS and GTK_LIBS, if gthread is specified in MODULES, 
dnl pass to pkg-config
dnl
AC_DEFUN(AM_PATH_GTK_2_0,
[dnl 
dnl Get the cflags and libraries from pkg-config
dnl
AC_ARG_ENABLE(gtktest, [  --disable-gtktest       do not try to compile and run a test GTK+ program],
		    , enable_gtktest=yes)

  pkg_config_args=gtk+-2.0
  for module in . $4
  do
      case "$module" in
         gthread) 
             pkg_config_args="$pkg_config_args gthread-2.0"
         ;;
      esac
  done

  no_gtk=""

  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

  if test x$PKG_CONFIG != xno ; then
    if pkg-config --atleast-pkgconfig-version 0.7 ; then
      :
    else
      echo *** pkg-config too old; version 0.7 or better required.
      no_gtk=yes
      PKG_CONFIG=no
    fi
  else
    no_gtk=yes
  fi

  min_gtk_version=ifelse([$1], ,2.0.0,$1)
  AC_MSG_CHECKING(for GTK+ - version >= $min_gtk_version)

  if test x$PKG_CONFIG != xno ; then
    ## don't try to run the test against uninstalled libtool libs
    if $PKG_CONFIG --uninstalled $pkg_config_args; then
	  echo "Will use uninstalled version of GTK+ found in PKG_CONFIG_PATH"
	  enable_gtktest=no
    fi

    if $PKG_CONFIG --atleast-version $min_gtk_version $pkg_config_args; then
	  :
    else
	  no_gtk=yes
    fi
  fi

  if test x"$no_gtk" = x ; then
    GTK_CFLAGS=`$PKG_CONFIG $pkg_config_args --cflags`
    GTK_LIBS=`$PKG_CONFIG $pkg_config_args --libs`
    gtk_config_major_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gtktest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GTK_CFLAGS"
      LIBS="$GTK_LIBS $LIBS"
dnl
dnl Now check if the installed GTK+ is sufficiently new. (Also sanity
dnl checks the results of pkg-config to some extent)
dnl
      rm -f conf.gtktest
      AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtktest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtk_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

  if ((gtk_major_version != $gtk_config_major_version) ||
      (gtk_minor_version != $gtk_config_minor_version) ||
      (gtk_micro_version != $gtk_config_micro_version))
    {
      printf("\n*** 'pkg-config --modversion gtk+-2.0' returned %d.%d.%d, but GTK+ (%d.%d.%d)\n", 
             $gtk_config_major_version, $gtk_config_minor_version, $gtk_config_micro_version,
             gtk_major_version, gtk_minor_version, gtk_micro_version);
      printf ("*** was found! If pkg-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK+. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
      printf("*** If pkg-config was wrong, set the environment variable PKG_CONFIG_PATH\n");
      printf("*** to point to the correct configuration files\n");
    } 
  else if ((gtk_major_version != GTK_MAJOR_VERSION) ||
	   (gtk_minor_version != GTK_MINOR_VERSION) ||
           (gtk_micro_version != GTK_MICRO_VERSION))
    {
      printf("*** GTK+ header files (version %d.%d.%d) do not match\n",
	     GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
      printf("*** library (version %d.%d.%d)\n",
	     gtk_major_version, gtk_minor_version, gtk_micro_version);
    }
  else
    {
      if ((gtk_major_version > major) ||
        ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
        ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK+ (%d.%d.%d) was found.\n",
               gtk_major_version, gtk_minor_version, gtk_micro_version);
        printf("*** You need a version of GTK+ newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK+ is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the pkg-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GTK+, but you can also set the PKG_CONFIG environment to point to the\n");
        printf("*** correct copy of pkg-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
      }
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes (version $gtk_config_major_version.$gtk_config_minor_version.$gtk_config_micro_version))
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$PKG_CONFIG" = "no" ; then
       echo "*** A new enough version of pkg-config was not found."
       echo "*** See http://pkgconfig.sourceforge.net"
     else
       if test -f conf.gtktest ; then
        :
       else
          echo "*** Could not run GTK+ test program, checking why..."
          CFLAGS="$CFLAGS $GTK_CFLAGS"
          LIBS="$LIBS $GTK_LIBS"
          AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
],      [ return ((gtk_major_version) || (gtk_minor_version) || (gtk_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK+ or finding the wrong"
          echo "*** version of GTK+. If it is not finding GTK+, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK+ was incorrectly installed"
          echo "*** or that you have moved GTK+ since it was installed. In the latter case, you"
          echo "*** may want to edit the pkg-config script: $PKG_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  rm -f conf.gtktest
])


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



