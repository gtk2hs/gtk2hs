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
C="$(${GHCPKG} list --simple-output --global $1)"
if test "$USERPKGCONF" = "yes"; then
	C="${C} $(${GHCPKG} list --simple-output --user $1)"
fi
if echo "${C}" | ${GREP} $1 > /dev/null 2> /dev/null
then
	$2=$(for pkg in ${C} ; do echo "${pkg}" | sed -e 's/^[[A-Za-z0-9-]]*-\([[0-9.]]*\)$/\1/' ; done | sort -r -n | head -n1)
	AC_MSG_RESULT([yes, version $$2])
else
	AC_MSG_ERROR([
Missing GHC package "$1". Install "$1" and re-run ./configure
If $1 is actually installed but per-user rather than globally and
you want to do a per-user install of gtk2hs then use:
./configure --with-user-pkgconf --prefix=$HOME/some/path])
fi
])

dnl GTKHS_PKG_CHECK(arg enable name, package name, package var name,
dnl                 pkg-config version requirements,
dnl                 internal dependency package var names,
dnl                 arg enable help string,
dnl                 error if false)
dnl perform the AC_ARG_ENABLE, PKG_CHECK_MODULES and AM_CONDITIONAL for a
dnl pkg-config module and corresponding gtk2hs package.
AC_DEFUN([GTKHS_PKG_CHECK],
[
# Check if user wants $1 bindings. Defaults to auto, or in packager
# mode it defaults to no.
AC_ARG_ENABLE($1,
	AS_HELP_STRING([--enable-$1],[$6]),
	[ENABLE_$3=[$]enableval],[ENABLE_$3=[$]ENABLE_PKG_DEFAULT])
for internaldep in $5 ; do
  if test "`eval echo [\$]\{ENABLE_[$]{internaldep}\}`" != "yes" ; then
    if test "[$]ENABLE_$3" = yes ; then
       AC_MSG_ERROR([
Another gtk2hs package that $2 depends on is not being built.
Generally this means that the gtk package cannot be built, but
you explicitly enabled one of the other packages.  Please make sure
you have all the prerequisites enabled.])
    else
      ENABLE_$3=no
    fi
  fi
done
if test "[$]ENABLE_$3" = "yes" || test "[$]ENABLE_$3" = "auto"; then
  PKG_CHECK_MODULES($3,$4,[ENABLE_$3=yes],
  		[if test "[$]ENABLE_$3" = "auto"; then
			ENABLE_$3=no
		 else
		 	AC_MSG_ERROR([$7])
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

dnl PROG_LD_X_FLAG
dnl
dnl Sets the output variable LD_X to -x if ld supports this flag, otherwise the
dnl variable's value is empty.
AC_DEFUN([PROG_LD_X],
[
echo 'foo() {}' > conftest.c
${CC} -c conftest.c
if ${LD} -r -x -o conftest2.o conftest.o > /dev/null 2>&1; then
   LD_X=-x
else
   LD_X=
fi
rm -f conftest.c conftest.o conftest2.o
AC_SUBST(LD_X)
])dnl LD_X

dnl PROG_LD_INPUT
dnl
dnl Sets the output variable LD_INPUT to yes or no, depending on whether it
dnl supports the @FILE style of reading commands from a file.
AC_DEFUN([PROG_LD_INPUT],
[
echo 'foo() {}' > conftest.c
${CC} -c conftest.c
echo conftest.o > conftest.list
if ${LD} -r -o conftest2.o @conftest.list > /dev/null 2>&1; then
  LD_INPUT=yes
else
  LD_INPUT=no
fi
rm -f conftest.c conftest.o conftest2.o conftest.list
AC_SUBST([LD_INPUT])
])dnl PROG_LD_INPUT

dnl These functions come with autoconf version 2.61 and higher. We include them here
dnl for backwards compatability with autoconf 2.59. This section should be removed
dnl once we require 2.61 or higher.

# AC_PROG_SED
# -----------
# Check for a fully functional sed program that truncates
# as few characters as possible.  Prefer GNU sed if found.
AC_DEFUN([AC_PROG_SED],
[AC_CACHE_CHECK([for a sed that does not truncate output], ac_cv_path_SED,
    [dnl ac_script should not contain more than 99 commands (for HP-UX sed),
     dnl but more than about 7000 bytes, to catch a limit in Solaris 8 /usr/ucb/sed.
     ac_script=s/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb/
     for ac_i in 1 2 3 4 5 6 7; do
       ac_script="$ac_script$as_nl$ac_script"
     done
     echo "$ac_script" | sed 99q >conftest.sed
     $as_unset ac_script || ac_script=
     _AC_PATH_PROG_FEATURE_CHECK(SED, [sed gsed],
	[_AC_FEATURE_CHECK_LENGTH([ac_path_SED], [ac_cv_path_SED],
		["$ac_path_SED" -f conftest.sed])])])
 SED="$ac_cv_path_SED"
 AC_SUBST([SED])dnl
 rm -f conftest.sed
])# AC_PROG_SED


# AC_PROG_GREP
# ------------
# Check for a fully functional grep program that handles
# the longest lines possible and which respects multiple -e options.
# Prefer GNU grep if found.
AC_DEFUN([AC_PROG_GREP],
[AC_CACHE_CHECK([for grep that handles long lines and -e], ac_cv_path_GREP,
   [_$0(GREP, [grep ggrep], [-e 'GREP$' -e '-(cannot match)-'])])
 GREP="$ac_cv_path_GREP"
 AC_SUBST([GREP])
])


# _AC_PROG_GREP(VARIABLE, PROGNAME-LIST, PROG-ARGUMENTS)
# ------------------------------------------------------
# Solaris 9 /usr/xpg4/bin/*grep is suitable, but /usr/bin/*grep lacks -e.
# AIX silently truncates long lines before matching.
# NeXT understands only one -e and truncates long lines.
m4_define([_AC_PROG_GREP],
[_AC_PATH_PROG_FEATURE_CHECK([$1], [$2],
	[_AC_FEATURE_CHECK_LENGTH([ac_path_$1], [ac_cv_path_$1],
		["$ac_path_$1" $3], [$1])], [$PATH$PATH_SEPARATOR/usr/xpg4/bin])
])


# _AC_PATH_PROG_FEATURE_CHECK(VARIABLE, PROGNAME-LIST, FEATURE-TEST, [PATH])
# --------------------------------------------------------------------------
# FEATURE-TEST is called repeatedly with $ac_path_VARIABLE set to the
# name of a program in PROGNAME-LIST found in PATH.  FEATURE-TEST must set
# $ac_cv_path_VARIABLE to the path of an acceptable program, or else
# _AC_PATH_PROG_FEATURE_CHECK will report that no acceptable program
# was found, and abort.  If a suitable $ac_path_VARIABLE is found in the
# FEATURE-TEST macro, it can set $ac_path_VARIABLE_found=':' to accept
# that value without any further checks.
m4_define([_AC_PATH_PROG_FEATURE_CHECK],
[# Extract the first word of "$2" to use in msg output
if test -z "$$1"; then
set dummy $2; ac_prog_name=$[2]
AC_CACHE_VAL([ac_cv_path_$1],
[ac_path_$1_found=false
# Loop through the user's path and test for each of PROGNAME-LIST
_AS_PATH_WALK([$4],
[for ac_prog in $2; do
  for ac_exec_ext in '' $ac_executable_extensions; do
    ac_path_$1="$as_dir/$ac_prog$ac_exec_ext"
    AS_EXECUTABLE_P(["$ac_path_$1"]) || continue
    $3
    $ac_path_$1_found && break 3
  done
done
])
])
$1="$ac_cv_path_$1"
if test -z "$$1"; then
  AC_MSG_ERROR([no acceptable $ac_prog_name could be found in dnl
m4_default([$4], [\$PATH])])
fi
AC_SUBST([$1])
else
  ac_cv_path_$1=$$1
fi
])


# _AC_FEATURE_CHECK_LENGTH(PROGPATH, CACHE-VAR, CHECK-CMD, [MATCH-STRING])
# ------------------------------------------------------------------------
# For use as the FEATURE-TEST argument to _AC_PATH_PROG_FEATURE_TEST.
# On each iteration run CHECK-CMD on an input file, storing the value
# of PROGPATH in CACHE-VAR if the CHECK-CMD succeeds.  The input file
# is always one line, starting with only 10 characters, and doubling
# in length at each iteration until approx 10000 characters or the
# feature check succeeds.  The feature check is called at each
# iteration by appending (optionally, MATCH-STRING and) a newline
# to the file, and using the result as input to CHECK-CMD.
m4_define([_AC_FEATURE_CHECK_LENGTH],
[# Check for GNU $1 and select it if it is found.
  _AC_PATH_PROG_FLAVOR_GNU([$$1],
    [$2="$$1" $1_found=:],
  [ac_count=0
  echo $ECHO_N "0123456789$ECHO_C" >"conftest.in"
  while :
  do
    cat "conftest.in" "conftest.in" >"conftest.tmp"
    mv "conftest.tmp" "conftest.in"
    cp "conftest.in" "conftest.nl"
    echo '$4' >> "conftest.nl"
    $3 < "conftest.nl" >"conftest.out" 2>/dev/null || break
    diff "conftest.out" "conftest.nl" >/dev/null 2>&1 || break
    ac_count=`expr $ac_count + 1`
    if test $ac_count -gt ${$1_max-0}; then
      # Best one so far, save it but keep looking for a better one
      $2="$$1"
dnl   # Using $1_max so that each tool feature checked gets its
dnl   # own variable.  Don't reset it otherwise the implied search
dnl   # for best performing tool in a list breaks down.
      $1_max=$ac_count
    fi
    # 10*(2^10) chars as input seems more than enough
    test $ac_count -gt 10 && break
  done
  rm -f conftest.in conftest.tmp conftest.nl conftest.out])
])


# _AC_PATH_PROG_FLAVOR_GNU(PROGRAM-PATH, IF-SUCCESS, [IF-FAILURE])
# ----------------------------------------------------------------
m4_define([_AC_PATH_PROG_FLAVOR_GNU],
[# Check for GNU $1
case `"$1" --version 2>&1` in
*GNU*)
  $2;;
m4_ifval([$3],
[*)
  $3;;
])esac
])# _AC_PATH_PROG_FLAVOR_GNU


