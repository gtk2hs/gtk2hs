TOP = ..

include $(TOP)/mk/config.mk

SUBDIRS	= abstract buttons display entry general layout menuComboToolbar \
	  misc multiline ornaments scrolling selectors \
	  treeList windows gdk glib pango ../compat

ifeq ($(WIN32),yes)
EXTRA_HFILES	= $(TOP)/gtk/wingtk.h
SUBSYSTEM	= windows
else
SUBDIRS += embedding
endif

EXTRA_TARFILES	= wingtk.h

PACKAGENAME = gtk2
LIBNAME = gtk2hs

MAIN = general/Gtk.hs

HEADER = gtk/gtk.h

EXCLUDE_DOCS = general/FFI.hs ../compat/LocalControl.hs ../compat/LocalData.hs
PREPROC_DOCS = general/Gtk.hs

# Whenever a variable module-HEADER is defined, the file module.chs
# is translated with a separate invokation of c2hs with the header
# file $module-HEADER. The header file in this variable should be
# the most specific header file so that c2hs needs to parse as little
# header source as possible. All modules for which module-HEADER is
# not defined will be compiled by c2hs with $HEADER as header file and
# all pending modules at once. This currently only works with the local
# copy of c2hs, which is why the following HEADER variables are only set
# if we compile with an external c2hs.

ifneq (x$(MULTIPLE_CHS),xyes)
# glib modules
GObject-HEADER		= glib-object.h
GType-HEADER		= glib-object.h
GValue-HEADER		= glib-object.h
GValueTypes-HEADER	= glib-object.h
GList-HEADER		= glib.h
# pango modules
PangoLayout-HEADER	= pango/pango-layout.h
PangoTypes-HEADER	= pango/pango.h
Rendering-HEADER	= pango/pango-context.h
# gdk modules
GdkEnums-HEADER		= gdk/gdk.h
GdkKeys-HEADER		= gdk/gdkkeys.h
Region-HEADER		= gdk/gdkregion.h
Drawable-HEADER		= gdk/gdkdrawable.h
GC-HEADER		= gdk/gdkgc.h
Keys-HEADER		= gdk/gdkkeys.h
Drawable-HEADER		= gdk/gdkdrawable.h
Pixbuf-HEADER		= gdk/gdkpixbuf.h
Gdk-HEADER		= gdk/gdk.h
# general Gtk modules
IconFactory-HEADER	= gtk/gtkiconfactory.h
Signal-HEADER		= gtk/gtksignal.h
Style-HEADER		= gtk/gtkstyle.h
Enums-HEADER		= gtk/gtk.h
General-HEADER		= gtk/gtk.h
Signal-HEADER		= gtk/gtk.h
Hierarchy-HEADER	= gtk/gtk.h
# windows
Window-HEADER		= gtk/gtkwindow.h
Dialog-HEADER		= gtk/gtkdialog.h
Plug-HEADER		= gtk/gtkplug.h
FileSel-HEADER		= gtk/gtkfilesel.h
# abstract
Object-HEADER		= gtk/gtkobject.h
Widget-HEADER		= gtk/gtkwidget.h
Container-HEADER	= gtk/gtkcontainer.h
Box-HEADER		= gtk/gtkbox.h
Paned-HEADER		= gtk/gtkpaned.h
Range-HEADER		= gtk/gtkrange.h
Scale-HEADER		= gtk/gtkscale.h
# display
AccelLabel-HEADER	= gtk/gtkaccellabel.h
Label-HEADER		= gtk/gtklabel.h
Image-HEADER		= gtk/gtkimage.h
ProgressBar-HEADER	= gtk/gtkprogressbar.h
Statusbar-HEADER	= gtk/gtkstatusbar.h
# buttons
Button-HEADER		= gtk/gtkbutton.h
CheckButton-HEADER	= gtk/gtkcheckbutton.h
RadioButton-HEADER	= gtk/gtkradiobutton.h
ToggleButton-HEADER	= gtk/gtktogglebutton.h
# entry
Entry-HEADER		= gtk/gtkentry.h
HScale-HEADER		= gtk/gtkhscale.h
VScale-HEADER		= gtk/gtkvscale.h
SpinButton-HEADER	= gtk/gtkspinbutton.h
# layout
Alignment-HEADER	= gtk/gtkalignment.h
AspectFrame-HEADER	= gtk/gtkaspectframe.h
HBox-HEADER		= gtk/gtkhbox.h
VBox-HEADER		= gtk/gtkvbox.h
HPaned-HEADER		= gtk/gtkhpaned.h
VPaned-HEADER		= gtk/gtkvpaned.h
Table-HEADER		= gtk/gtktable.h
Layout-HEADER		= gtk/gtklayout.h
Notebook-HEADER		= gtk/gtknotebook.h
# menu and combo
Combo-HEADER		= gtk/gtk.h
CheckMenuItem-HEADER	= gtk/gtkcheckmenuitem.h
MenuShell-HEADER	= gtk/gtkmenushell.h
MenuBar-HEADER		= gtk/gtkmenubar.h
Menu-HEADER		= gtk/gtkmenu.h
MenuItem-HEADER		= gtk/gtkmenuitem.h
ImageMenuItem-HEADER	= gtk/gtkimagemenuitem.h
RadioMenuItem-HEADER	= gtk/gtkradiomenuitem.h
OptionMenu-HEADER	= gtk/gtkoptionmenu.h
TearoffMenuItem-HEADER	= gtk/gtktearoffmenuitem.h
Toolbar-HEADER		= gtk/gtktoolbar.h
# multiline
TextBuffer-HEADER	= gtk/gtktextbuffer.h
TextIter-HEADER		= gtk/gtktextiter.h
  # gtktextmark.h does not include gobject.h but needs it
TextMark-HEADER		= gtk/gtk.h
TextTag-HEADER		= gtk/gtktexttag.h
TextTagTable-HEADER	= gtk/gtktexttagtable.h
TextView-HEADER		= gtk/gtktextview.h
# ornaments
Frame-HEADER		= gtk/gtkframe.h
HSeparator-HEADER	= gtk/gtkhseparator.h
VSeparator-HEADER	= gtk/gtkvseparator.h
# scrolling
HScrollbar-HEADER	= gtk/gtkhscrollbar.h
ScrolledWindow-HEADER	= gtk/gtkscrolledwindow.h
VScrollbar-HEADER	= gtk/gtkvscrollbar.h
# misc
Adjustment-HEADER	= gtk/gtkadjustment.h
Arrow-HEADER		= gtk/gtkarrow.h
Calendar-HEADER		= gtk/gtkcalendar.h
DrawingArea-HEADER	= gtk/gtkdrawingarea.h
Socket-HEADER		= gtk/gtksocket.h
Misc-HEADER		= gtk/gtkmisc.h
EventBox-HEADER		= gtk/gtkeventbox.h
Calendar-HEADER		= gtk/gtkcalendar.h
HandleBox-HEADER	= gtk/gtkhandlebox.h
  # gtkwidget.h is not included but needed by gtktooltips.h, thus use gtk.h
Tooltips-HEADER		= gtk/gtk.h
Viewport-HEADER		= gtk/gtkviewport.h
# trees and lists
TreeSelection-HEADER	= gtk/gtktreeselection.h
TreeStore-HEADER	= gtk/gtktreestore.h
TreeView-HEADER		= gtk/gtktreeview.h
TreeViewColumn-HEADER	= gtk/gtktreeviewcolumn.h
TreeModel-HEADER	= gtk/gtktreemodel.h
TreeModelSort-HEADER	= gtk/gtktreemodelsort.h
CellRenderer-HEADER	= gtk/gtkcellrenderer.h
CellRendererText-HEADER	= gtk/gtkcellrenderertext.h
CellRendererPixbuf-HEADER = gtk/gtkcellrendererpixbuf.h
CellRendererToggle-HEADER = gtk/gtkcellrenderertoggle.h
ListStore-HEADER	= gtk/gtkliststore.h
# extensions
Pixbuf-HEADER		= gtk/gtk.h 
# replace last line with gdk-pixbuf/gdk-pixbuf.h
endif

# These are .chi files that need to be created before any other .chs files.
# This is a very poor way of expressing dependencies.

NEEDCHI = general/Hierarchy general/Signal glib/GValue glib/GList \
	  glib/GObject pango/PangoTypes \
	  treeList/TreeModel treeList/TreeViewColumn multiline/TextIter \
	  gdk/Region

NEEDPACKAGES = data

# options that are needed when compiling this or with this package:
EXTRAHC_FLAGS = -fglasgow-exts 

# Further options to the C preprocessor are passed verbatim:
EXTRA_CPPFLAGS	= $(GTK_CFLAGS)

EXTRA_LIBS	= $(GTK_LIBS)

# These symbols are need to break a dependency circle.
#\"-u TreeSelection_dwzw\",\"-u TreeSelection_dwzx\"
# EXTRA_SYMBOLS = TreeSelection_dwzw TreeSelection_dwzx 

# the signal marshallers are generated automatically
EXTRA_CHSFILES	+= general/Signal.chs
EXTRA_STUBFILES += general/Signal.chs

ifeq (x$(BROKENCB),xyes)
ENABLEBROKEN	= --broken
else
ENABLEBROKEN	=
endif

general/Signal.chs : $(TOP)/tools/callbackGen/Signal.chs-boot1 \
		     $(TOP)/tools/callbackGen/Signal.chs-boot2 \
		     $(TOP)/tools/callbackGen/gtkmarshal.list
	$(strip $(TOP)/tools/callbackGen/HookGenerator $(MARSHALLDEFS) \
	$(TOP)/tools/callbackGen/ general/Signal.chs $(ENABLEBROKEN))

EXTRA_CHSFILES	+=  general/Hierarchy.chs

ifeq ($(WIN32),yes)
CREATE_TYPES = default
else
CREATE_TYPES = plugNsocket default
endif

ifeq ($(GTK_VERSION_2_2),yes)
CREATE_TYPES += gtk-2.2
endif
ifeq ($(GTK_VERSION_2_4),yes)
CREATE_TYPES += gtk-2.4
endif
ifeq ($(DISABLE_DEPRECATED),no)
CREATE_TYPES += deprecated
endif

ifeq ($(DISABLE_DEPRECATED),yes)
C2HSFLAGS += -C-DG_DISABLE_DEPRECATED -C-DGDK_PIXBUF_DISABLE_DEPRECATED\
 -C-DGDK_DISABLE_DEPRECATED -C-DGTK_DISABLE_DEPRECATED
endif

general/Hierarchy.chs : $(TOP)/tools/hierarchyGen/hierarchy.list
	$(strip $(TOP)/tools/hierarchyGen/TypeGenerator \
	$(TOP)/tools/hierarchyGen/hierarchy.list \
	general/Hierarchy.chs $(addprefix --tag=,$(CREATE_TYPES)))

include $(TOP)/mk/common.mk
