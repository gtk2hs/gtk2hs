#!/bin/sh
# Return only flags that refer to existing directories and those 
# that are not yet in CFLAGS or LDFLAGS.
# Usage:
# ./checkDirs.sh {-I<dir1> | -L<dir2> | <flag>}
#
if test -z "$SED"; then SED=sed; fi;
if test -z "$GREP"; then GREP=grep; fi;
RES=; 
for FLAG in $@; do
  case $FLAG in 
	-L*) DIR=`echo $FLAG | $SED "s/-L//"`;
	     if test -d $DIR; then 
		if test -z "`echo $LDFLAGS | $GREP -e $FLAG`"; then 
	 	  RES="$RES $FLAG"; fi;
	     fi;;
	-I*) DIR=`echo $FLAG | $SED "s/-I//"`;
             if test -d $DIR; then
		if test -z "`echo $CFLAGS | $GREP -e $FLAG`"; then 
	 	  RES="$RES $FLAG"; fi;
	     fi;;
	*) if test -z "`echo $CFLAGS $LDFLAGS | $GREP -e $FLAG`"; then
	  RES="$RES $FLAG"; fi;;
  esac;
done;
echo $RES;

