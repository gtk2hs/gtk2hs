dnl gtk2hs - Additional macros for `autoconf'

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
AC_DEFUN([GTKHS_GHC_VERSION],
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
AC_DEFUN([GTKHS_PROG_CHECK_VERSION],
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
fi])dnl

dnl GTKHS_REFORMAT_PACKAGE_CFLAGS(CFLAGS, CFLAGS_CQ)
dnl 
dnl for ghc package.conf files, we need to convert from
dnl   CFLAGS = -DFOO -I/usr/include/bar -I/usr/include/baz
dnl to
dnl   CFLAGS_CQ = "/usr/include/bar","/usr/include/baz"
dnl
AC_DEFUN([GTKHS_REFORMAT_PACKAGE_CFLAGS],
[
C=; [$2]=;
for FLAG in [$][$1]; do
  case [$]FLAG in
    -I*) [$2]="[$][$2][$]C\"[$]{FLAG#-I}\""; C=",";;
  esac;
done;
])dnl

dnl GTKHS_REFORMAT_PACKAGE_LIBS(LIBS, LIBS_CQ, LIBDIR_CQ, LIBEXTRA_CQ)
dnl 
dnl for ghc package.conf files, we need to convert from
dnl   LIBS = -Wl,--export-dynamic -lfoo -lbar -I/usr/lib/foo -I/usr/lib/bar
dnl to
dnl   LIBS_CQ = "foo", "bar"
dnl   LIBDIR_CQ = "/usr/lib/bar","/usr/lib/bar"
dnl   LIBEXTRA_CQ = "-Wl,--export-dynamic"
dnl
AC_DEFUN([GTKHS_REFORMAT_PACKAGE_LIBS],
[
C_LIBS=; [$2]=; C_LDIR=; [$3]=; C_XTRA=; [$4]=; 
for FLAG in [$][$1]; do
  case [$]FLAG in
    -l*) [$2]="[$][$2][$]C_LIBS\"[$]{FLAG#-l}\""; C_LIBS=",";;
    -L*) [$3]="[$][$3][$]C_LDIR\"[$]{FLAG#-I}\""; C_LDIR=",";;
    *)   [$4]="[$][$4][$]C_XTRA\"[$]FLAG\""; C_XTRA=",";;
  esac;
done;
])dnl
