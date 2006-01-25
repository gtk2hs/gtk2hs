
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
packages = glib pango atk gtk glade gconf sourceview

gtk_releases_url   = ftp://ftp.gtk.org/pub/gtk/v2.8
gnome_platform_url = http://ftp.gnome.org/pub/gnome/platform/2.12/2.12.2/sources
gnome_desktop_url  = http://ftp.gnome.org/pub/gnome/desktop/2.12/2.12.2/sources

glib_version = 2.8.6
glib_dir = glib
glib_tar = glib-$(glib_version).tar.bz2
glib_url = $(gtk_releases_url)/$(glib_tar)

pango_version = 1.10.2
pango_dir = pango
pango_tar = pango-$(pango_version).tar.bz2
pango_url = $(gtk_releases_url)/$(pango_tar)

atk_version = 1.10.3
atk_dir = atk
atk_tar = atk-$(atk_version).tar.bz2
atk_url = $(gtk_releases_url)/$(atk_tar)

gtk_version = 2.8.10
gtk_dir = gtk
gtk_tar = gtk+-$(gtk_version).tar.bz2
gtk_url = $(gtk_releases_url)/$(gtk_tar)

glade_version = 2.5.1
glade_dir = glade
glade_tar = libglade-$(glade_version).tar.bz2
glade_url = $(gnome_platform_url)/$(glade_tar)

gconf_version = 2.12.1
gconf_dir = gconf
gconf_tar = GConf-$(gconf_version).tar.bz2
gconf_url = $(gnome_platform_url)/$(gconf_tar)

gnomecanvas_version = 2.12.0
gnomecanvas_dir = gnomecanvas
gnomecanvas_tar = libgnomecanvas-$(gnomecanvas_version).tar.bz2
gnomecanvas_url = $(gnome_platform_url)/$(gnomecanvas_tar)

sourceview_version = 1.4.2
sourceview_dir = sourceview
sourceview_tar = gtksourceview-$(sourceview_version).tar.bz2
sourceview_url = $(gnome_desktop_url)/$(sourceview_tar)

vte_version = 0.11.15
vte_dir = vte
vte_tar = vte-$(vte_version).tar.bz2
vte_url = $(gnome_desktop_url)/$(vte_tar)

gtkhtml2_version = 2.6.3
gtkhtml2_dir = gtkhtml2
gtkhtml2_tar = libgtkhtml-$(gtkhtml2_version).tar.bz2
gtkhtml2_url = $(gnome_desktop_url)/$(gtkhtml2_tar)

gtkhtml3_version = 3.8.1
gtkhtml3_dir = gtkhtml3
gtkhtml3_tar = gtkhtml-$(gtkhtml3_version).tar.bz2
gtkhtml3_url = $(gnome_desktop_url)/$(gtkhtml3_tar)

gnome_version = 2.12.0.1
gnome_dir = gnome
gnome_tar = libgnome-$(gnome_version).tar.bz2
gnome_url = $(gnome_platform_url)/$(gnome_tar)

gnomeui_version = 2.12.0
gnomeui_dir = gnomeui
gnomeui_tar = libgnomeui-$(gnomeui_version).tar.bz2
gnomeui_url = $(gnome_platform_url)/$(gnomeui_tar)

gnomeprint_version = 2.10.0
gnomeprint_dir = gnomeprint
gnomeprint_tar = libgnomeprint-$(gnomeprint_version).tar.bz2
gnomeprint_url = $(gnome_platform_url)/$(gnomeprint_tar)

gnomeprintui_version = 2.10.0.1
gnomeprintui_dir = gnomeprintui
gnomeprintui_tar = libgnomeprintui-$(gnomeprintui_version).tar.bz2
gnomeprintui_url = $(gnome_platform_url)/$(gnomeprintui_tar)


gnome_db_url = http://ftp.gnome.org/pub/gnome/sources/$(1)/1.3

gda_version = 1.3.1
gda_dir = gda
gda_tar = libgda-$(gda_version).tar.bz2
gda_url = $(call gnome_db_url,libgda)/$(gda_tar)

gnomedb_version = 1.3.1
gnomedb_dir = gnomedb
gnomedb_tar = libgnomedb-$(gnomedb_version).tar.bz2
gnomedb_url = $(call gnome_db_url,libgnomedb)/$(gnomedb_tar)

gtkglext_version = 1.0.6
gtkglext_dir = gtkglext
gtkglext_tar = gtkglext-$(gtkglext_version).tar.bz2
gtkglext_url = http://heanet.dl.sourceforge.net/sourceforge/gtkglext/$(gtkglext_tar)


#following fixes borrowed from Gtk#
gtk-post-download-hook = \
	ln -f -s gtkfilechooserprivate.h gtk/gtk/gtkfilechooserpriv.h

atk-post-download-hook = \
	sed -i 's:( \*link_activated):(\* link_activated):' atk/atk/atkhyperlink.h


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
	wget -c $($*_url) --output-document=tars/$($*_tar) && touch $@

% : tars/%.stamp
	tar -xjf tars/$($*_tar)
	mv $($*_tar:.tar.bz2=) $*
	touch $*
	$($*-post-download-hook)

.PHONY: get-source-code
get-source-code: $(packages)

.PRECIOUS : %-api.xml
%-api.xml : %-sources.xml gapi_format_xml
	PATH=.:$$PATH ./gapi_parser.pl $<

.PRECIOUS : %-docs.xml
%-docs.xml : format-docs.xsl
	./mkdocxml.sh $(if $($*_docs_standalone),--standalone) $($*_docs_dirs) \
		| xsltproc format-docs.xsl - > $@

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

ApiGen : ApiGen.hs Api.hs Docs.hs FormatDocs.hs CodeGen.hs Marshal.hs \
	MarshalFixup.hs ModuleScan.hs ExcludeApi.hs StringUtils.hs
	$(HC) --make $< -o $@
HC=ghc

gapi_format_xml : formatXml.c
	gcc `pkg-config --cflags --libs libxml-2.0 glib-2.0` $< -o $@

########################
#
#       other stuff
#

clean :
	rm *.o *.hi ApiGen gapi_format_xml
	rm *-api.xml *-docs.xml
