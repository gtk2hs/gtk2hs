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

dnl GHC_PKG_CHECK(PKG_NAME)
AC_DEFUN([GHC_PKG_CHECK],
[
AC_MSG_CHECKING([for the GHC package "$1"])
if ${GHCPKG} --simple-output list $1 > /dev/null 2> /dev/null
then
	AC_MSG_RESULT(yes)
else
	AC_MSG_ERROR([
Missing GHC package "$1". Install "$1" and re-run ./configure])
fi
])

dnl GTKHS_PKG_CHECK(arg enable name, package name, package var name,
dnl                 pkg-config version requirements,
dnl                 arg enable help string,
dnl                 error if false)
dnl perform the AC_ARG_ENABLE, PKG_CHECK_MODULES and AM_CONDITIONAL for a
dnl pkg-config module and corresponding gtk2hs package.
AC_DEFUN([GTKHS_PKG_CHECK],
[
# Check if user wants $1 bindings. Defaults to auto, or in packager
# mode it defaults to no.
AC_ARG_ENABLE($1,
	AS_HELP_STRING([--enable-$1],[$5]),
	[ENABLE_$3=[$]enableval],[ENABLE_$3=[$]ENABLE_PKG_DEFAULT])

if test "[$]ENABLE_$3" = "yes" || test "[$]ENABLE_$3" = "auto"; then
  PKG_CHECK_MODULES($3,$4,[ENABLE_$3=yes],
  		[if test "[$]ENABLE_$3" = "auto"; then
			ENABLE_$3=no
		 else
		 	AC_MSG_ERROR([$6])
		 fi])
fi
AC_MSG_CHECKING([whether to build $2 package])
AC_MSG_RESULT([$]ENABLE_$3)
AM_CONDITIONAL(ENABLE_$3, test "[$]ENABLE_$3" = "yes")
if test "[$]ENABLE_$3" = "yes"; then
  AC_DEFINE(ENABLE_$3, [], [Whether or not the $1 package is available.])
fi
])dnl

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
SEP=", "; QUOTE="\""
for FLAG in [$][$1]; do
  case [$]FLAG in
    -I*) [$2]="[$][$2][$]C[$]QUOTE[$]{FLAG#-I}$QUOTE"; C=$SEP;;
  esac;
done;
])dnl

dnl GTKHS_REFORMAT_PACKAGE_LIBS(LIBS, LIBS_CQ, LIBDIR_CQ, LIBEXTRA_CQ)
dnl 
dnl for ghc package.conf files, we need to convert from
dnl   LIBS = -Wl,--export-dynamic -lfoo -lbar -L/usr/lib/foo -L/usr/lib/bar
dnl to
dnl   LIBS_CQ = "foo", "bar"
dnl   LIBDIR_CQ = "/usr/lib/bar","/usr/lib/bar"
dnl   LIBEXTRA_CQ = "-Wl,--export-dynamic"
dnl
AC_DEFUN([GTKHS_REFORMAT_PACKAGE_LIBS],
[
C_LIBS=; [$2]=; C_LDIR=; [$3]=; C_XTRA=; [$4]=; 
SEP=","; QUOTE="\""
for FLAG in [$][$1]; do
  case [$]FLAG in
    -l*) [$2]="[$][$2][$]C_LIBS[$]QUOTE[$]{FLAG#-l}$QUOTE"; C_LIBS=$SEP;;
    -L*) [$3]="[$][$3][$]C_LDIR[$]QUOTE[$]{FLAG#-L}$QUOTE"; C_LDIR=$SEP;;
    *)   [$4]="[$][$4][$]C_XTRA[$]QUOTE[$]FLAG[$]QUOTE"; C_XTRA=$SEP;;
  esac;
done;
dnl Fix for ghc-pkg in that it doesn't differ between paths to extra
dnl libraries and paths to Haskell libraries. We have to append the
dnl path to where the Haskell libraries are going to be installed in
dnl case they go into a non-standard directory. If they go into a
dnl standard directory then we duplicate a path here. Dough.
[$3]="[$][$3][$]C_LDIR[$]QUOTE\${pkglibdir}[$]QUOTE";
])dnl

dnl Another hack, on glibc systems GHCi cannot load the pthread library,
dnl so do not include it in the LIBS list. This is not usually a problem since
dnl some other lib usually has the pthread library as a dependency and the
dnl system dynamic linker loads up the pthread library automatically.
AC_DEFUN([GTKHS_GLIBC_PTHREAD_HACK],
[
TMP_[$1]=;
for FLAG in [$][$1]; do
  case [$]FLAG in
    -lpthread) TMP_[$1]="[$]TMP_[$1]";;
    -lz)       TMP_[$1]="[$]TMP_[$1]";;
    *)         TMP_[$1]="[$]TMP_[$1] [$]FLAG";;
  esac;
done;
[$1]=[$]TMP_[$1]
])

dnl GTKHS_PACKAGE_FILTER_CFLAGS(RESULT_CFLAGS, FILTER_CFLAGS, INPUT_CFLAGS)
dnl
dnl Assign to RESULT_CFLAGS only the INPUT_CFLAGS that refer to existing
dnl directories and those that are not yet in the FILTER_CFLAGS.
AC_DEFUN([GTKHS_PACKAGE_FILTER_CFLAGS],
[
RES=;
for FLAG in $3; do
  case [$]FLAG in
	-I*) if test -d [$]{FLAG#-I}; then
	       if test -z "`echo $2 | [$]GREP -x -e [$]{FLAG}`"; then
	         RES="[$]RES [$]FLAG"; fi;
	     fi;;
	*) if test -z "`echo $2 | [$]GREP -x -e [$]{FLAG}`"; then
	     RES="[$]RES [$]FLAG"; fi;;
  esac
done;
$1=`echo [$]RES`;
])

dnl GTKHS_PACKAGE_FILTER_LIBS(RESULT_LIBS, FILTER_LIBS, INPUT_LIBS)
dnl
dnl Assign to RESULT_LIBS only the INPUT_LIBS that refer to existing
dnl directories and those that are not yet in the FILTER_LIBS.
AC_DEFUN([GTKHS_PACKAGE_FILTER_LIBS],
[
RES=;
for FLAG in $3; do
  case [$]FLAG in
	-L*) if test -d [$]{FLAG#-L}; then
	       if test -z "`echo $2 | [$]GREP -e [$]FLAG`"; then
	         RES="[$]RES [$]FLAG"; fi;
	     fi;;
	*) if test -z "`echo $2 | [$]GREP -e [$]FLAG`"; then
	     RES="[$]RES [$]FLAG"; fi;;
  esac;
done;
$1=`echo [$]RES`;
])
