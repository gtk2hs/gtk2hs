#!/bin/bash

DOCBOOKDIR=../apicoverage/tars/gtk+-2.4.13/docs/reference/gtk/xml
HEADDERS=/usr/include/gtk-2.0/gtk/*.h

mkdirhier doc api modules
echo > modules/missing_docs

for HEADDER in $HEADDERS
do
	APIFILE=api/$(basename $HEADDER).xml
	DOCBOOKFRAG=$DOCBOOKDIR/$(basename ${HEADDER%.h}).xml
	DOCBOOKFILE=doc/$(basename ${HEADDER%.h}).docbook
	DOCFILE=doc/$(basename ${HEADDER%.h}).xml
	echo Processing $HEADDER
	
	./gapi_pp.pl $HEADDER | ./gapi2xml.pl Gtk $APIFILE gtk+ >> /dev/null || exit
#	./gapi_format_xml $APIFILE.tmp $APIFILE || exit
	rm  $APIFILE.tmp

        if test -f $DOCBOOKFRAG; then
		cat <(echo \
		'<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.1.2//EN"
		"http://www.oasis-open.org/docbook/xml/4.1.2/docbookx.dtd">
		<book>') $DOCBOOKFRAG <(echo '</book>') \
		> $DOCBOOKFILE || exit
		echo "xsltproc format-docs.xsl $DOCBOOKFILE > $DOCFILE"
		xsltproc format-docs.xsl $DOCBOOKFILE > $DOCFILE || exit

		echo ./ApiGen $APIFILE Template.chs --doc=$DOCFILE --outdir=modules
		./ApiGen $APIFILE Template.chs --doc=$DOCFILE --outdir=modules || exit
	else
		echo ./ApiGen $APIFILE Template.chs --outdir=modules
		./ApiGen $APIFILE Template.chs --outdir=modules || exit
		echo $HEADDER: could not find $DOCBOOKFRAG >> modules/missing_docs
	fi
done

