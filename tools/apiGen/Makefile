
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
	@echo "make prep-gtk-docs"
	@echo "  This configures and compiles gtk+ and builds the DocBook xml"
	@echo "  files necessary for the next stage. You should only have to"
	@echo "  do this once."
	@echo
	@echo "make gtk-modules"
	@echo "  This generates all the Gtk modules by processing the source"
	@echo "  code and the DocBook documentation. The .chs modules are all"
	@echo "  put into the subdirectory gtk-modules. You need to have"
	@echo "  downloaded the source and built the docs before doing this."
	@echo
	@echo "make gdk-modules"
	@echo "  as above but for Gdk"
	@echo
	@echo "make prep-pango-docs pango-modules"
	@echo "  as above but for Pango"
	@echo
	@echo "make prep-glade-docs glade-modules"
	@echo "  as above but for libglade"

########################
#
#	source code
#
PANGO_VERSION = 1.2.3
GTK_VERSION = 2.2.2
GLADE_VERSION = 2.0.0

DOWNLOADS = \
	http://ftp.gnome.org/pub/GNOME/desktop/2.2/2.2.2/sources/pango-$(PANGO_VERSION).tar.gz	\
	http://ftp.gnome.org/pub/GNOME/desktop/2.2/2.2.2/sources/gtk+-$(GTK_VERSION).tar.gz	\
	http://ftp.gnome.org/pub/GNOME/desktop/2.0/2.0.0/sources/libglade-$(GLADE_VERSION).tar.gz

get-source-code:
	for i in $(DOWNLOADS); do                          \
		wget $$i --output-document=- | tar -xz ;   \
	done;


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
prep-gtk-docs : gtk+-$(GTK_VERSION)
	cd $< && ./configure --enable-gtk-doc && make

gtk-docs.xml : gtk+-$(GTK_VERSION)/docs/reference/gtk/xml
	./mkdocxml.sh $< | xsltproc format-docs.xsl - > $@

gtk-modules : gtk-api.xml gtk-docs.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --doc=gtk-docs.xml --outdir=$@


###################
#
#	Gdk modules
#
gdk-docs.xml :	gtk+-$(GTK_VERSION)/docs/reference/gdk/xml \
		gtk+-$(GTK_VERSION)/docs/reference/gdk-pixbuf/xml
	./mkdocxml.sh $< | xsltproc format-docs.xsl - > $@

gdk-modules : gdk-api.xml gdk-docs.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --doc=gdk-docs.xml --outdir=$@


###################
#
#	Pango modules
#
prep-pango-docs : pango-$(PANGO_VERSION)
	cd $< && ./configure --enable-gtk-doc && make

pango-docs.xml : pango-$(PANGO_VERSION)/docs/xml
	./mkdocxml.sh $< | xsltproc format-docs.xsl - > $@

pango-modules : pango-api.xml pango-docs.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --doc=pango-docs.xml --outdir=$@


########################
#
#	libglade modules
#
prep-glade-docs : libglade-$(GLADE_VERSION)
	cd $< && ./configure --enable-gtk-doc && make

glade-modules : glade-api.xml Template.chs ApiGen
	@mkdir -p $@
	./ApiGen $< Template.chs --outdir=$@


########################
#
#	tools
#
ApiGen : ApiGen.hs
	ghc --make $< -o $@

gapi_format_xml : formatXml.c
	gcc `pkg-config --cflags --libs libxml-2.0 glib-2.0` $< -o $@
