#!/bin/sh
# Return only flags that refer to existing directories and those 
# that are not yet in CFLAGS or LDFLAGS.
# Usage:
# ./checkDirs.sh {-I<dir1> | -L<dir2> | <flag>}
#
RES=; 
for FLAG in $@; do
  case $FLAG in 
	-L*) DIR=`echo $FLAG | sed "s/-L//"`;
	     if test -d $DIR; then 
		if test -z "`echo $LDFLAGS | grep -e $FLAG`"; then 
	 	  RES="$RES $FLAG"i; fi;
	     fi;;
	-I*) DIR=`echo $FLAG | sed "s/-I//"`;
             if test -d $DIR; then
		if test -z "`echo $CFLAGS | grep -e $FLAG`"; then 
	 	  RES="$RES $FLAG"; fi;
	     fi;;
	*) if test -z "`echo $CFLAGS $LDFLAGS | grep -e $FLAG`"; then
	  RES="$RES $FLAG"; fi;;
  esac;
done;
echo $RES;

