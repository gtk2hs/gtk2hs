
default : 
	@echo "== gtk2hs apiGen system =="
	@echo "available targets:"
	@echo
	@echo "make get-source-code"
	@echo "  This downloads all the necessary source code from the Gnome"
	@echo "  ftp server. You must do this before you can use any of the"
	@echo "  other options. However, you may wish to first edit this"
	@echo "  Makefile to change which versions of the source code you get."
	@echo
	@echo "make gtk-modules"
	@echo "  This generates all the Gtk modules by processing the source"
	@echo "  code and the DocBook documentation. The .chs modules are all"
	@echo "  put into the subdirectory gtk-modules. You need to have"
	@echo "  downloaded the source and before doing this."
	@echo
	@echo "make gdk-modules"
	@echo "  as above but for Gdk"
	@echo
	@echo "make pango-modules"
	@echo "  as above but for Pango"
	@echo
	@echo "make glade-modules"
	@echo "  as above but for libglade"

########################
#
#	source code
#
GLIB_VERSION = 2.4.8
PANGO_VERSION = 1.4.1
ATK_VERSION = 1.6.1
GTK_VERSION = 2.4.14
GLADE_VERSION = 2.3.6

DOWNLOADS = \
	ftp://ftp.gtk.org/pub/gtk/v2.4/glib-$(GLIB_VERSION).tar.gz				\
	ftp://ftp.gtk.org/pub/gtk/v2.4/pango-$(PANGO_VERSION).tar.gz				\
	ftp://ftp.gtk.org/pub/gtk/v2.4/atk-$(ATK_VERSION).tar.gz				\
	ftp://ftp.gtk.org/pub/gtk/v2.4/gtk+-$(GTK_VERSION).tar.gz				\
	http://ftp.gnome.org/pub/GNOME/desktop/2.6/2.6.2/sources/libglade-$(GLADE_VERSION).tar.gz\
	http://ftp.gnome.org/pub/GNOME/desktop/2.6/2.6.2/sources/libgnomecanvas-2.6.1.1.tar.gz

get-source-code:
	for i in $(DOWNLOADS); do                          \
		wget $$i --output-document=- | tar -xz ;   \
	done;
	# following fixes borrowed from Gtk#
	ln -f -s gtkfilechooserprivate.h gtk+-$(GTK_VERSION)/gtk/gtkfilechooserpriv.h
	sed -i 's:( \*link_activated):(\* link_activated):' atk-1.6.1/atk/atkhyperlink.h

#############################
#
#	generateing api files
#
%-api.xml : %-sources.xml gapi_format_xml
	PATH=.:$$PATH ./gapi_parser.pl $<

#%-modules : %-api.xml %-docs.xml Template.chs ApiGen
#	@mkdir -p $@
#	./ApiGen $< Template.chs --doc=gtk-docs.xml --outdir=$@


###################
#
#	Gtk modules
#
#prep-gtk-docs : gtk+-$(GTK_VERSION)
#	cd $< && ./configure --enable-gtk-doc && make

gtk-docs.xml : gtk+-$(GTK_VERSION)/docs/reference/gtk/xml format-docs.xsl
	./mkdocxml.sh $< | xsltproc format-docs.xsl - > $@

gtk-modules : gtk-api.xml gtk-docs.xml Template.chs ApiGen \
		gdk-api.xml pango-api.xml atk-api.xml
	@mkdir -p $@
	./ApiGen $< Template.chs --doc=gtk-docs.xml --outdir=$@ \
	--includeapi=gdk-api.xml --includeapi=pango-api.xml --includeapi=atk-api.xml \
	--modprefix=Graphics.UI.Gtk.{-Category-}


###################
#
#	Gdk modules
#
gdk-docs.xml :	gtk+-$(GTK_VERSION)/docs/reference/gdk/xml \
		gtk+-$(GTK_VERSION)/docs/reference/gdk-pixbuf/xml \
		format-docs.xsl
	./mkdocxml.sh $< | xsltproc format-docs.xsl - > $@

gdk-modules : gdk-api.xml gdk-docs.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --doc=gdk-docs.xml --outdir=$@


###################
#
#	Pango modules
#
#prep-pango-docs : pango-$(PANGO_VERSION)
#	cd $< && ./configure --enable-gtk-doc && make

pango-docs.xml : pango-$(PANGO_VERSION)/docs/xml format-docs.xsl
	./mkdocxml.sh $< | xsltproc format-docs.xsl - > $@

pango-modules : pango-api.xml pango-docs.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --doc=pango-docs.xml --outdir=$@


########################
#
#	libglade modules
#
glade-modules : glade-api.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --outdir=$@


########################
#
#	gnomecanvas modules
#
gnomecanvas-modules : gnomecanvas-api.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --outdir=$@


########################
#
#	tools
#
ApiGen : ApiGen.hs Api.hs Docs.hs FormatDocs.hs \
	Marshal.hs CodeGen.hs StringUtils.hs 
	ghc --make $< -o $@

gapi_format_xml : formatXml.c
	gcc `pkg-config --cflags --libs libxml-2.0 glib-2.0` $< -o $@
