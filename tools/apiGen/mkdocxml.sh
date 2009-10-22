#!/bin/bash

# This script sticks a bunch of DocBook fragments together into one xml file.
# The result is *not* valid DocBook xml! But it is the right input format for
# the format-docs.xsl program which munges the DocBook into the format
# accepted by the ApiGen program.

echo '<!DOCTYPE  refentry PUBLIC "-//OASIS//DTD DocBook XML V4.3//EN"
     "http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd" [
     <!ENTITY gdk-pixbuf "<application>gdk-pixbuf</application>">
     <!ENTITY nbsp        "&#x00A0;">
     <!ENTITY mdash       "&#x2014;">
     <!ENTITY times       "&#x00D7;">
     <!ENTITY percnt      "&#x0025;">
     <!ENTITY num "&#x0023;">
     <!ENTITY lt  "&#38;#60;">
     <!ENTITY gt  "&#x003E;">
     <!ENTITY ast "&#x002A;">
     <!ENTITY sol "&#x002F;">
     <!ENTITY commat "&#x0040;">
     <!ENTITY ldquo "&#x201C;" ><!--=double quotation mark, left-->
     <!ENTITY rdquo "&#x201D;" ><!--=double quotation mark, right-->
     ]>'
echo "<apidoc>"

case $1 in
  --standalone)
  	shift 1
	xsltproc extract-docs.xsl $(find $@ -name '*.xml');;

  *)	for DOC in $(find $@ -name '*.xml')
	do
		if `grep -q "<refentry" $DOC`; then
                        NUM=`grep -m1 -n "<refentry" $DOC | sed "s/\([0-9]*\):.*/\1/"`;
                        echo "<book filename=\"$(basename $DOC)\">"
		        tail -n+$NUM $DOC
                        #echo $DOC from line $NUM;
		        echo "</book>"
                fi;
	done;;
esac

echo "</apidoc>"
