TOP = ..

include $(TOP)/mk/config.mk

SUBDIRS	= abstract buttons display entry general layout menuComboToolbar \
	  misc multiline ornaments scrolling \
	  treeList windows gdk glib pango ../compat

ifeq (x$(WIN32),xyes)
EXTRA_HFILES	= $(TOP)/gtk/wingtk.h
SUBSYSTEM	= windows
else
SUBDIRS += embedding
endif

EXTRA_TARFILES	= $(TOP)/gtk/wingtk.h

PACKAGENAME = gtk2
LIBNAME = gtk2hs

INSTALLDIR = gtk2hs

MAIN = general/Gtk.hs

HEADER = gtk/gtk.h

ifneq (x$(MULTIPLE_CHS),xyes)
# general and non-Gtk
GObject-HEADER		= glib-object.h
GType-HEADER		= glib-object.h
GValue-HEADER		= glib-object.h
GValueTypes-HEADER	= glib-object.h
GList-HEADER		= glib.h
GdkEnums-HEADER		= gdk/gdk.h
GdkKeys-HEADER		= gdk/gdkkeys.h
IconFactory-HEADER	= gtk/gtkiconfactory.h
Signal-HEADER		= gtk/gtksignal.h
Enums-HEADER		= gtk/gtk.h
General-HEADER		= gtk/gtk.h
Region-HEADER		= gdk/gdkregion.h
Drawable-HEADER		= gdk/gdkdrawable.h
GC-HEADER		= gdk/gdkgc.h
Keys-HEADER		= gdk/gdkkeys.h
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

endif

# These are .chi files that need to be created before any other .chs files.
# This is a very poor way of expressing dependencies.

NEEDCHI = general/Hierarchy general/Signal glib/GValue glib/GList \
	  treeList/TreeModel treeList/TreeViewColumn multiline/TextIter \
	  gdk/Region

ifneq (x$(NEW_MODULE_SYS),xyes)
NEEDPACKAGES = data lang
else
NEEDPACKAGES = data
endif

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

general/Signal.chs : signals/HookGenerator $(MARSHALLDEFS)\
	    	  signals/Signal.chs-boot1 signals/Signal.chs-boot2
	$(strip signals/HookGenerator $(MARSHALLDEFS) signals/ \
	general/Signal.chs $(ENABLEBROKEN))

EXTRA_CLEANFILES += signals/HookGenerator signal/HookGenerator.o signal/Main.hi

EXTRA_TARFILES	+= signals/HookGenerator.hs signals/Makefile \
		   signals/gtkmarshal.list signals/Signal.chs-boot1 \
		   signals/Signal.chs-boot2

signals/HookGenerator : signals/HookGenerator.hs 
	$(MAKE) -C signals

EXTRA_CHSFILES	+=  general/Hierarchy.chs

ifeq ($(WIN32),yes)
IGNORE_TYPES	= Plug Socket
else
IGNORE_TYPES	=
endif

general/Hierarchy.chs : typehier/TypeGenerator typehier/hierarchy.list
	$(strip typehier/TypeGenerator typehier/hierarchy.list \
	general/Hierarchy.chs $(IGNORE_TYPES))

EXTRA_CLEANFILES += typehier/TypeGenerator typehier/TypeGenerator.o \
		 typehier/Main.hi typehier/hierarchy.list

EXTRA_TARFILES	+= typehier/TypeGenerator.hs typehier/Makefile \
		   typehier/hierarchy.list

typehier/TypeGenerator : typehier/TypeGenerator.hs
	$(MAKE) -C typehier

include $(TOP)/mk/common.mk
