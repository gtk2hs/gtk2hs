
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
	@echo
	@echo "make clean"
	@echo "  Removes files from apiGen build."
	@echo
	@echo "make clean-all"
	@echo "  Removes all created files, including downloaded tarballs."

########################
#
#	source code
#
packages = glib pixbuf pango atk gtk glade gconf sourceview vte

gnome_sources_url = http://ftp.gnome.org/pub/gnome/sources

glib_version = 2.22
glib_step = 2
glib_dir = glib
glib_tar = glib-$(glib_version).$(glib_step).tar.bz2
glib_url = $(gnome_sources_url)/$(glib_dir)/$(glib_version)/$(glib_tar)

pixbuf_version = 0.22
pixbuf_step = 0
pixbuf_dir = gdk-pixbuf
pixbuf_tar = gdk-pixbuf-$(pixbuf_version).$(pixbuf_step).tar.bz2
pixbuf_url = $(gnome_sources_url)/$(pixbuf_dir)/$(pixbuf_version)/$(pixbuf_tar)

pango_version = 1.26
pango_step = 0
pango_dir = pango
pango_tar = pango-$(pango_version).$(pango_step).tar.bz2
pango_url = $(gnome_sources_url)/$(pango_dir)/$(pango_version)/$(pango_tar)

atk_version = 1.28
atk_step = 0
atk_dir = atk
atk_tar = atk-$(atk_version).$(atk_step).tar.bz2
atk_url = $(gnome_sources_url)/$(atk_dir)/$(atk_version)/$(atk_tar)

gtk_version = 2.18
gtk_step = 3
gtk_dir = gtk+
gtk_tar = gtk+-$(gtk_version).$(gtk_step).tar.bz2
gtk_url = $(gnome_sources_url)/$(gtk_dir)/$(gtk_version)/$(gtk_tar)

glade_version = 2.12
glade_step = 2
glade_dir = glade
glade_tar = glade-$(glade_version).$(glade_step).tar.bz2
glade_url = $(gnome_sources_url)/$(glade_dir)/$(glade_version)/$(glade_tar)

gconf_version = 2.28
gconf_step = 0
gconf_dir = GConf
gconf_tar = GConf-$(gconf_version).$(gconf_step).tar.bz2
gconf_url = $(gnome_sources_url)/$(gconf_dir)/$(gconf_version)/$(gconf_tar)

sourceview_version = 2.9
sourceview_step = 1
sourceview_dir = gtksourceview
sourceview_tar = gtksourceview-$(sourceview_version).$(sourceview_step).tar.bz2
sourceview_url = $(gnome_sources_url)/$(sourceview_dir)/$(sourceview_version)/$(sourceview_tar)

vte_version = 0.22
vte_step = 2
vte_dir = vte
vte_tar = vte-$(vte_version).$(vte_step).tar.bz2
vte_url = $(gnome_sources_url)/$(vte_dir)/$(vte_version)/$(vte_tar)

gtkglext_version = 1.2.0
gtkglext_dir = gtkglext
gtkglext_tar = gtkglext-$(gtkglext_version).tar.bz2
gtkglext_url = http://heanet.dl.sourceforge.net/sourceforge/gtkglext/$(gtkglext_tar)


#following fixes borrowed from Gtk#
gtk-post-download-hook = \
	ln -f -s gtkfilechooserprivate.h gtk/gtk/gtkfilechooserpriv.h

atk-post-download-hook = \
	gnused -i 's:( \*link_activated):(\* link_activated):' atk/atk/atkhyperlink.h


###################
#
#	Gtk modules
#

gtk_docs_dirs   = gtk/docs/reference/gtk/xml
gtk_includeapi  = gdk-api.xml pango-api.xml atk-api.xml \
		  gobject-api.xml glib-api.xml
gtk_excludeapi  = gtk.ignore
gtk_modprefix   = Graphics.UI.Gtk.{-Category-}
gtk_scanmodules = gtk/Graphics/UI/Gtk
gtk_excludescan = gtk/Graphics/UI/Gtk/Pango gtk/Graphics/UI/Gtk/Gdk

gtk-modules : gtk-docs.xml $(gtk_includeapi) $(gtk_excludeapi)


###################
#
#	Gdk modules
#

gdk_docs_dirs   = gtk/docs/reference/gdk/xml gtk/docs/reference/gdk-pixbuf/xml
gdk_includeapi  = pango-api.xml gobject-api.xml glib-api.xml
gdk_excludeapi  = gtk.ignore
gdk_modprefix   = Graphics.UI.Gtk.Gdk
gdk_scanmodules = gtk/Graphics/UI/Gtk/Gdk
gdk-modules : gdk-docs.xml


###################
#
#	Pixbuf modules
#

pixbuf_docs_dirs   = pixbuf/doc/xml
pixbuf_modprefix   = Graphics.UI.Gtk.Gdk
pixbuf_scanmodules = gtk/Graphics/UI/Gtk/Gdk
pixbuf-modules : pixbuf-docs.xml

###################
#
#	Pango modules
#

pango_docs_dirs   = pango/docs/xml
pango_modprefix   = Graphics.UI.Gtk.Pango
pango_scanmodules = gtk/Graphics/UI/Gtk/Pango
pango-modules : pango-docs.xml


###################
#
#       Glib modules
#
#

glib_docs_dirs   = glib/docs/reference/glib/xml
glib_modprefix   = System.Glib
glib_scanmodules = glib/System/Glib
glib-modules : glib-api.xml glib-docs.xml Template.chs ApiGen

gobject_docs_dirs = glib/docs/reference/gobject/xml
gobject_modprefix   = System.Glib
gobject_scanmodules = glib/System/Glib
gobject-modules : gobject-docs.xml


########################
#
#	glade modules
#

glade_docs_dirs   = glade/doc/xml
glade_docs_standalone = yes
glade_includeapi = gtk-api.xml $(gtk_includeapi)
glade_excludeapi = gtk.ignore
glade_modprefix   = Graphics.UI.Gtk
glade_scanmodules = glade/Graphics/UI/Gtk
glade-modules : glade-docs.xml


########################
#
#	gnomecanvas modules
#

gnomecanvas_docs_dirs = gnomecanvas/docs/reference/xml
gnomecanvas_includeapi = gtk-api.xml $(gtk_includeapi)
gnomecanvas_excludeapi = gtk.ignore
gnomecanvas-modules : gnomecanvas-docs.xml


########################
#
#	gconf modules
#

gconf_docs_dirs   = gconf/doc/gconf/xml
gconf_docs_standalone = yes
gconf_includeapi  = gobject-api.xml glib-api.xml
gconf_excludeapi  = gtk.ignore
gconf_modprefix   = System.Gnome.GConf
gconf_scanmodules = gconf/System/Gnome/GConf
gconf-modules : gconf-docs.xml


########################
#
#       sourceview modules
#

sourceview_docs_dirs   = sourceview/docs/reference/xml
sourceview_docs_standalone = yes
sourceview_includeapi  = gtk-api.xml $(gtk_includeapi)
sourceview_excludeapi  = gtk.ignore
sourceview_modprefix   = Graphics.UI.Gtk.SourceView
sourceview_scanmodules = sourceview/Graphics/UI/Gtk/SourceView
sourceview-modules : sourceview-docs.xml


########################
#
#       gnome modules
#

gnome_docs_dirs   = gnome/doc/reference/xml
gnome_includeapi = gconf-api.xml gobject-api.xml glib-api.xml
gnome_excludeapi = gtk.ignore
gnome-modules : gnome-docs.xml

gnomeui_docs_dirs = gnomeui/doc/reference/xml
gnomeui_includeapi = gtk-api.xml $(gtk_includeapi) gnome-api.xml $(gnome_includeapi)
gnomeui_excludeapi = gtk.ignore
gnomeui-modules : gnomeui-docs.xml


########################
#
#       gnomeprint modules
#

gnomeprint_docs_dirs  = gnomeprint/doc/reference/xml
gnomeprint_includeapi = pango-api.xml gobject-api.xml glib-api.xml
gnomeprint_excludeapi = gtk.ignore
gnomeprint-modules : gnomeprint-docs.xml

gnomeprintui_docs_dirs = gnomeprintui/doc/xml
gnomeprintui_includeapi = gnomeprint-api.xml $(gnomeprint_includeapi) \
			  gnomecanvas-api.xml gtk-api.xml $(gtk_includeapi)
gnomeprintui_excludeapi = gtk.ignore
gnomeprintui-modules : gnomeprintui-docs.xml


########################
#
#       gnomedb modules
#

gda_docs_dirs  = gda/doc/C/xml
gda_includeapi = gobject-api.xml glib-api.xml
gda_excludeapi = gtk.ignore
gda-modules : gda-docs.xml

gnomedb_docs_dirs = gnomedb/doc/C/xml
gnomedb_includeapi = gda-api.xml gtk-api.xml $(gtk_includeapi)
gnomedb_excludeapi = gtk.ignore
gnomedb-modules : gnomedb-docs.xml


########################
#
#       vte modules
#

vte_docs_dirs = vte/doc/reference/xml
vte_includeapi = gtk-api.xml gdk-api.xml pango-api.xml atk-api.xml \
		 gobject-api.xml glib-api.xml
vte_excludeapi  = gtk.ignore
vte_modprefix   = Graphics.UI.Gtk.Vte
vte_scanmodules = vte/Graphics/UI/Gtk/Vte/
vte-modules : vte-docs.xml


########################
#
#       html modules
#

gtkhtml2_includeapi = gtk-api.xml gdk-api.xml pango-api.xml atk-api.xml \
		      gobject-api.xml glib-api.xml
gtkhtml2_excludeapi = gtk.ignore

gtkhtml3_includeapi = $(gtkhtml2_includeapi)
gtkhtml3_excludeapi = $(gtkhtml2_excludeapi)


########################
#
#       gtkglext modules
#
#

gtkglext_docs_dirs   = gtkglext/docs/reference/gtkglext/xml
gtkglext_includeapi  = gdkglext-api.xml gtk-api.xml gdk-api.xml pango-api.xml \
                       gobject-api.xml glib-api.xml
gtkglext_excludeapi  = gtk.ignore
gtkglext_prefix      = gtk
gtkglext_modprefix   = Graphics.UI.Gtk.OpenGL
gtkglext_scanmodules = gtkglext/Graphics/UI/Gtk/OpenGL
gtkglext-modules : gtkglext-docs.xml

#gdkglext_docs_dirs   = gtkglext/docs/reference/gtkglext/xml
gdkglext_includeapi  = gdk-api.xml pango-api.xml \
                       gobject-api.xml glib-api.xml
gdkglext_excludeapi  = gtk.ignore
gdkglext_prefix      = gdk
gdkglext_modprefix   = Graphics.UI.Gtk.Gdk.OpenGL
gdkglext_scanmodules = gtkglext/Graphics/UI/Gtk/Gdk/OpenGL
gdkglext-modules : gdkglext-docs.xml


#######################
#
#       Dock widget from libegg
#

eggdock-modules : eggdock-api.xml Template.chs ApiGen \
	gtk-api.xml
	@mkdir -p $@
	./ApiGen $< Template.chs --outdir=$@ \
	--includeapi=gtk-api.xml --includeapi=gobject-api.xml \
	--includeapi=glib-api.xml \
	--excludeapi=gtk.ignore \
	--modprefix=Graphics.UI.Gtk.Egg.Dock


#############################
#
#	general rules
#

.PRECIOUS : tars/%.stamp
tars/%.stamp :
	mkdir -p tars
	wget -c $($*_url) --output-document=tars/$($*_tar) && touch $@

pkg-% : tars/%.stamp
	tar -xjf tars/$($*_tar)
	mv $($*_tar:.tar.bz2=) $*
	touch $*
	$($*-post-download-hook)

.PHONY: get-source-code
get-source-code: $(addprefix pkg-,$(packages))

.PRECIOUS : %-api.xml
%-api.xml : %-sources.xml gapi_format_xml
	chmod ug+x gapi_parser.pl
	chmod ug+x gapi_pp.pl
	chmod ug+x gapi2xml.pl
	PATH=.:$$PATH ./gapi_parser.pl $<

.PRECIOUS : %-docs.xml
%-docs.xml : format-docs.xsl
	chmod ug+x mkdocxml.sh 
	./mkdocxml.sh $(if $($*_docs_standalone),--standalone) $($*_docs_dirs) \
		| xsltproc --novalid format-docs.xsl - > $@

%-modules : %-api.xml % Template.chs ApiGen
	@mkdir -p $@
	$(strip ./ApiGen $< Template.chs --outdir=$@ \
		$(if $($*_docs_dirs), --doc=$*-docs.xml) \
		$(addprefix --includeapi=,$($*_includeapi)) \
		$(addprefix --excludeapi=,$($*_excludeapi)) \
		$(addprefix --modprefix=,$($*_modprefix)) \
		$(addprefix --scanmodules=$(TOP)/,$($*_scanmodules)) \
		$(addprefix --excludescan=$(TOP)/,$($*_excludescan)))
	@touch $@

TOP = ../..

########################
#
#	tools
#

ApiGen : $(addprefix src/,ApiGen.hs Api.hs Docs.hs AddDocs.hs HaddockDocs.hs \
	CodeGen.hs Marshal.hs MarshalFixup.hs ModuleScan.hs ExcludeApi.hs \
	Utils.hs Module.hs Names.hs)
	$(HC) -isrc --make $< -o $@
HC=ghc

gapi_format_xml : formatXml.c
	gcc `pkg-config --cflags --libs libxml-2.0 glib-2.0` $< -o $@

########################
#
#       other stuff
#

clean :
	rm src/*.o src/*.hi ApiGen gapi_format_xml || true
	rm *-api.xml *-docs.xml || true
clean-all : clean
	rm -r atk gconf glade gtk glib sourceview pango || true
	rm -r gtk-modules tars glade-modules pango-modules || true
