AUTOMAKE_OPTIONS = foreign subdir-objects
SUFFIXES = .chs.pp .chs .hsc .deps .dep
MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES = *.precomp
CPPFLAGS += $(filter -I% -D%, \
	$(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS) \
	$(LIBGLADE_CFLAGS) $(GCONF_CFLAGS))

# Build c2hs before anything else.

SOURCEDIRS = gtk/general gtk/glib gtk/pango gtk/treeList gtk/multiline	     \
	gtk/gdk gtk/abstract gtk/display gtk/entry gtk/misc gtk/multiline    \
	gtk/ornaments gtk/scrolling gtk/treeList gtk/selectors gtk/embedding \
	gtk/layout gtk/menuComboToolbar gtk/buttons gtk/windows		     \
	tools/hierarchyGen tools/callbackGen tools/apicoverage		     \
	tools/c2hs/base/admin tools/c2hs/base/errors			     \
	tools/c2hs/base/general tools/c2hs/base/graphs			     \
	tools/c2hs/base/state tools/c2hs/base/syms			     \
	tools/c2hs/base/syntax tools/c2hs/base/sysdep tools/c2hs/c	     \
	tools/c2hs/chs tools/c2hs/gen tools/c2hs/state tools/c2hs/toplevel   \
	mogul glade gconf gconf/System/Gnome gconf/System/Gnome/GConf	     \
	sourceview

EXTRA_DIST = \
	tools/callbackGen/Signal.chs-boot1	\
	tools/callbackGen/Signal.chs-boot2	\
	tools/callbackGen/gtkmarshal.list	\
	tools/checkDirs.sh			\
	tools/c2hs/toplevel/c2hs_config.h	\
	tools/hierarchyGen/hierarchy.list

# fixme: this should be in configure.ac:
HSCPP = $(CPP) -x c -traditional-cpp -P

CHSDEPEND = $(srcdir)/mk/chsDepend

HSC = hsc2hs

# all packages and applications
lib_LIBRARIES =		\
        libHSgtk.a	\
        libHSmogul.a	\
        libHSglade.a	\
        libHSgconf.a	\
        libHSsourceview.a

noinst_PROGRAMS = \
	tools/hierarchyGen/TypeGenerator \
	tools/callbackGen/HookGenerator \
	tools/apicoverage/Exclude \
	tools/c2hs/c2hsLocal

#
#	TypeGenerator Tool
#
################################################################################

# While building the program <name>, set the variable NAME to <name> so
# we can access program-specific variables like <name>_PACKAGEDEPS. The
# following is a hack to prevent automake from assuming that we are overriding
# the tools/hierarchyGen/TypeGenerator goal.
TypeGenerator_NAME = tools/hierarchyGen/TypeGenerator
$(TypeGenerator_NAME) : NAME = tools_hierarchyGen_TypeGenerator

tools_hierarchyGen_TypeGenerator_MAIN = \
  $(tools_hierarchyGen_TypeGenerator_SOURCES)
tools_hierarchyGen_TypeGenerator_PACKAGEDEPS = 
tools_hierarchyGen_TypeGenerator_SOURCES = \
  tools/hierarchyGen/TypeGen.hs
# Fix automake - the subdir-objects option doesn't work here.
am_tools_hierarchyGen_TypeGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_hierarchyGen_TypeGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_hierarchyGen_TypeGenerator_OBJECTS)

gtk/general/Hierarchy.chs : $(srcdir)/tools/hierarchyGen/hierarchy.list \
			    $(srcdir)/tools/hierarchyGen/TypeGenerator
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
        $(srcdir)/tools/hierarchyGen/hierarchy.list \
        $@ $(addprefix --tag=,$(CREATE_TYPES)))

#
#	HookGenerator Tool
#
################################################################################

HookGenerator_NAME = tools/callbackGen/HookGenerator
$(HookGenerator_NAME) : NAME = tools_callbackGen_HookGenerator

tools_callbackGen_HookGenerator_MAIN = \
  $(tools_callbackGen_HookGenerator_SOURCES)
tools_callbackGen_HookGenerator_PACKAGEDEPS = 
tools_callbackGen_HookGenerator_SOURCES = \
  tools/callbackGen/HookGenerator.hs
am_tools_callbackGen_HookGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_callbackGen_HookGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_callbackGen_HookGenerator_OBJECTS)


gtk/general/Signal.chs : $(srcdir)/tools/callbackGen/Signal.chs-boot1 \
			 $(srcdir)/tools/callbackGen/Signal.chs-boot2 \
			 $(srcdir)/tools/callbackGen/gtkmarshal.list \
			 $(srcdir)/tools/callbackGen/HookGenerator
	$(strip $(srcdir)/tools/callbackGen/HookGenerator $(MARSHALLDEFS) \
	$(srcdir)/tools/callbackGen/ gtk/general/Signal.chs \
	$(if $(subst yes,,$(BROKENCB)),--broken))

#
#	API Coverage Tool
#
################################################################################

Exclude_NAME = tools/apicoverage/Exclude
$(Exclude_NAME) : NAME = tools_apicoverage_Exclude

tools_apicoverage_Exclude_MAIN = \
  $(tools_apicoverage_Exclude_SOURCES)
tools_apicoverage_Exclude_PACKAGEDEPS = 
tools_apicoverage_Exclude_SOURCES = \
  tools/apicoverage/Exclude.hs
am_tools_apicoverage_Exclude_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_apicoverage_Exclude_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_apicoverage_Exclude_OBJECTS)

#
#	c2hs interface generator
#
################################################################################

tools_c2hs_c2hsLocal_NAME = tools/c2hs/c2hsLocal
$(tools_c2hs_c2hsLocal_NAME) : NAME = tools_c2hs_c2hsLocal

tools_c2hs_c2hsLocal_MAIN = c2hs/toplevel/Main.hs
tools_c2hs_c2hsLocal_PACKAGEDEPS	= 
tools_c2hs_c2hsLocal_HEADER		= tools/c2hs/toplevel/c2hs_config.h
tools_c2hs_c2hsLocal_HCFLAGS		= -fglasgow-exts -fffi

tools_c2hs_c2hsLocal_SOURCES =			\
	tools/c2hs/base/admin/BaseVersion.hs	\
	tools/c2hs/base/admin/Common.hs		\
	tools/c2hs/base/admin/Config.hs		\
	tools/c2hs/base/errors/Errors.hs	\
	tools/c2hs/base/general/DLists.hs	\
	tools/c2hs/base/general/FileOps.hs	\
	tools/c2hs/base/general/FNameOps.hs	\
	tools/c2hs/base/general/GetOpt.hs	\
	tools/c2hs/base/general/UNames.hs	\
	tools/c2hs/base/general/Utils.hs	\
	tools/c2hs/base/general/Binary.hs	\
	tools/c2hs/base/general/FastMutInt.hs	\
	tools/c2hs/base/graphs/Marks.hs		\
	tools/c2hs/base/state/CIO.hs		\
	tools/c2hs/base/state/DynArrays.hs	\
	tools/c2hs/base/state/StateBase.hs	\
	tools/c2hs/base/state/State.hs		\
	tools/c2hs/base/state/StateTrans.hs	\
	tools/c2hs/base/syms/Attributes.hs	\
	tools/c2hs/base/syms/Idents.hs		\
	tools/c2hs/base/syms/NameSpaces.hs	\
	tools/c2hs/base/syntax/Lexers.hs	\
	tools/c2hs/base/syntax/ParserMonad.hs	\
	tools/c2hs/base/syntax/Parsers.hs	\
	tools/c2hs/base/syntax/Pretty.hs	\
	tools/c2hs/base/sysdep/SysDep.hs	\
	tools/c2hs/base/sysdep/SysDepPosix.hs	\
	tools/c2hs/c/CAST.hs			\
	tools/c2hs/c/CAttrs.hs			\
	tools/c2hs/c/CBuiltin.hs		\
	tools/c2hs/c/C.hs			\
	tools/c2hs/c/CLexer.hs			\
	tools/c2hs/c/CNames.hs			\
	tools/c2hs/c/CParser.hs			\
	tools/c2hs/c/CPretty.hs			\
	tools/c2hs/c/CTrav.hs			\
	tools/c2hs/chs/CHS.hs			\
	tools/c2hs/chs/CHSLexer.hs		\
	tools/c2hs/gen/CInfo.hs			\
	tools/c2hs/gen/GBMonad.hs		\
	tools/c2hs/gen/GenBind.hs		\
	tools/c2hs/gen/GenHeader.hs		\
	tools/c2hs/state/C2HSState.hs		\
	tools/c2hs/state/Switches.hs		\
	tools/c2hs/toplevel/Main.hs		\
	tools/c2hs/toplevel/Version.hs		\
	tools/c2hs/toplevel/C2HSConfig.hs	\
	tools/c2hs/toplevel/c2hs_config.c

tools_c2hs_c2hsLocal_HSFILES		= \
	$(filter %.hs,$(tools_c2hs_c2hsLocal_SOURCES))
# Fix automake - the subdir-objects option doesn't work here.
am_tools_c2hs_c2hsLocal_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(tools_c2hs_c2hsLocal_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_c2hs_c2hsLocal_OBJECTS)
MOSTLYCLEANFILES+= $(tools_c2hs_c2hsLocal_HSFILES:.hs=.hi)
CLEANFILES+= $(tools_c2hs_c2hsLocal_BUILDSOURCES)
DISTCLEANFILES+= tools_c2hs_c2hsLocal.deps
ifneq ($(MAKECMDGOALS),clean)
-include tools_c2hs_c2hsLocal.deps
 endif

#
#	gtk package
#
################################################################################

libHSgtk_a_NAME = libHSgtk.a
$(libHSgtk_a_NAME) : NAME = libHSgtk_a

libHSgtk_a_PACKAGE		= gtk/gtk.pkg
libHSgtk_a_PACKAGEDEPS		= 
libHSgtk_a_HEADER 		= gtk/gtk.h
libHSgtk_a_PRECOMP		= gtk/gtk.precomp
libHSgtk_a_LIBS			= @GTK_LIBS@
libHSgtk_a_HCFLAGS		= -fglasgow-exts
libHSgtk_a_CFLAGS 		= $(filter-out -I% -D%,$(GTK_CFLAGS))
libHSgtk_a_CPPFLAGS 		= $(filter -I% -D%,$(GTK_CFLAGS))

libHSgtk_a_SOURCES =				  \
	gtk/general/Hierarchy.chs		  \
	gtk/general/Signal.chs			  \
	gtk/glib/GValue.chs			  \
	gtk/glib/GList.chs			  \
	gtk/glib/GObject.chs.pp			  \
	gtk/pango/PangoTypes.chs.pp		  \
	gtk/treeList/TreeModel.chs.pp		  \
	gtk/treeList/TreeViewColumn.chs		  \
	gtk/multiline/TextIter.chs.pp		  \
	gtk/gdk/Region.chs.pp			  \
	gtk/abstract/Bin.chs			  \
	gtk/abstract/Box.chs			  \
	gtk/abstract/ButtonBox.chs.pp		  \
	gtk/abstract/Container.chs		  \
	gtk/abstract/FileChooser.chs		  \
	gtk/abstract/Misc.chs			  \
	gtk/abstract/Object.chs.pp		  \
	gtk/abstract/Paned.chs			  \
	gtk/abstract/Range.chs			  \
	gtk/abstract/Scale.chs			  \
	gtk/abstract/Widget.chs			  \
	gtk/buttons/Button.chs.pp		  \
	gtk/buttons/CheckButton.chs		  \
	gtk/buttons/RadioButton.chs		  \
	gtk/buttons/ToggleButton.chs		  \
	gtk/display/AccelLabel.chs		  \
	gtk/display/Image.chs			  \
	gtk/display/Label.chs			  \
	gtk/display/ProgressBar.chs		  \
	gtk/display/Statusbar.chs		  \
	gtk/entry/Editable.chs			  \
	gtk/entry/Entry.chs.pp			  \
	gtk/entry/EntryCompletion.chs.pp	  \
	gtk/entry/HScale.chs			  \
	gtk/entry/SpinButton.chs		  \
	gtk/entry/VScale.chs			  \
	gtk/general/Enums.chs.pp		  \
	gtk/general/General.chs			  \
	gtk/general/IconFactory.chs.pp		  \
	gtk/general/Style.chs			  \
	gtk/layout/Alignment.chs.pp		  \
	gtk/layout/AspectFrame.chs		  \
	gtk/layout/Expander.chs.pp		  \
	gtk/layout/Fixed.chs			  \
	gtk/layout/HBox.chs			  \
	gtk/layout/HButtonBox.chs		  \
	gtk/layout/HPaned.chs			  \
	gtk/layout/Layout.chs			  \
	gtk/layout/Notebook.chs.pp		  \
	gtk/layout/Table.chs			  \
	gtk/layout/VBox.chs			  \
	gtk/layout/VButtonBox.chs		  \
	gtk/layout/VPaned.chs			  \
	gtk/menuComboToolbar/CheckMenuItem.chs.pp \
	gtk/menuComboToolbar/Combo.chs.pp	  \
	gtk/menuComboToolbar/ComboBox.chs.pp	  \
	gtk/menuComboToolbar/ComboBoxEntry.chs.pp \
	gtk/menuComboToolbar/ImageMenuItem.chs	  \
	gtk/menuComboToolbar/Menu.chs.pp	  \
	gtk/menuComboToolbar/MenuBar.chs	  \
	gtk/menuComboToolbar/MenuItem.chs	  \
	gtk/menuComboToolbar/MenuShell.chs	  \
	gtk/menuComboToolbar/OptionMenu.chs.pp	  \
	gtk/menuComboToolbar/RadioMenuItem.chs	  \
	gtk/menuComboToolbar/TearoffMenuItem.chs  \
	gtk/menuComboToolbar/ToolItem.chs.pp	  \
	gtk/menuComboToolbar/Toolbar.chs.pp	  \
	gtk/misc/Adjustment.chs			  \
	gtk/misc/Calendar.chs.pp		  \
	gtk/misc/DrawingArea.chs		  \
	gtk/misc/EventBox.chs.pp		  \
	gtk/misc/FileChooserWidget.chs.pp	  \
	gtk/misc/GArrow.chs			  \
	gtk/misc/HandleBox.chs			  \
	gtk/misc/SizeGroup.chs			  \
	gtk/misc/Tooltips.chs.pp		  \
	gtk/misc/Viewport.chs			  \
	gtk/multiline/TextBuffer.chs		  \
	gtk/multiline/TextMark.chs		  \
	gtk/multiline/TextTag.chs.pp		  \
	gtk/multiline/TextTagTable.chs		  \
	gtk/multiline/TextView.chs		  \
	gtk/ornaments/Frame.chs			  \
	gtk/ornaments/HSeparator.chs		  \
	gtk/ornaments/VSeparator.chs		  \
	gtk/scrolling/HScrollbar.chs		  \
	gtk/scrolling/ScrolledWindow.chs	  \
	gtk/scrolling/VScrollbar.chs		  \
	gtk/selectors/ColorSelection.chs	  \
	gtk/selectors/ColorSelectionDialog.chs	  \
	gtk/selectors/FontSelection.chs		  \
	gtk/selectors/FontSelectionDialog.chs	  \
	gtk/treeList/CellRendererPixbuf.chs	  \
	gtk/treeList/CellRendererText.chs	  \
	gtk/treeList/CellRendererToggle.chs	  \
	gtk/treeList/ListStore.chs.pp		  \
	gtk/treeList/TreeModelSort.chs		  \
	gtk/treeList/TreeSelection.chs		  \
	gtk/treeList/TreeStore.chs.pp		  \
	gtk/treeList/TreeView.chs.pp		  \
	gtk/windows/Dialog.chs			  \
	gtk/windows/FileChooserDialog.chs.pp	  \
	gtk/windows/FileSel.chs			  \
	gtk/windows/Window.chs.pp		  \
	gtk/gdk/Drawable.chs.pp			  \
	gtk/gdk/GC.chs				  \
	gtk/gdk/Gdk.chs				  \
	gtk/gdk/GdkEnums.chs			  \
	gtk/gdk/Keys.chs			  \
	gtk/gdk/Pixbuf.chs			  \
	gtk/glib/GError.chs.pp			  \
	gtk/glib/GType.chs			  \
	gtk/glib/GValueTypes.chs		  \
	gtk/pango/Description.chs		  \
	gtk/pango/PangoEnums.chs		  \
	gtk/pango/PangoLayout.chs		  \
	gtk/pango/Rendering.chs			  \
	gtk/embedding/Plug.chs			  \
	gtk/embedding/Socket.chs		  \
	gtk/general/StockItems.hsc		  \
	gtk/general/Structs.hsc			  \
	gtk/treeList/StoreValue.hsc		  \
	gtk/gdk/Events.hsc			  \
	gtk/glib/GParameter.hsc			  \
	gtk/embedding/Embedding.hsc		  \
	gtk/abstract/Scrollbar.hs		  \
	gtk/abstract/Separator.hs		  \
	gtk/general/FFI.hs			  \
	gtk/general/Gtk.hs			  \
	gtk/treeList/CellRenderer.hs		  \
	gtk/gdk/DrawWindow.hs			  \
	gtk/pango/Markup.hs

libHSgtk_a_LIBADD =						       \
	gtk/abstract/Container_stub.o				       \
	gtk/multiline/TextIter_stub.o gtk/entry/EntryCompletion_stub.o \
        gtk/multiline/TextTagTable_stub.o gtk/general/General_stub.o   \
        gtk/treeList/TreeModel_stub.o gtk/general/Signal_stub.o	       \
        gtk/treeList/TreeSelection_stub.o gtk/glib/GObject_stub.o      \
        gtk/treeList/TreeView_stub.o

# Pretend these stub files can be created by a simple rule.
gtk/abstract/Container_stub.o				       \
	gtk/multiline/TextIter_stub.o gtk/entry/EntryCompletion_stub.o \
        gtk/multiline/TextTagTable_stub.o gtk/general/General_stub.o   \
        gtk/treeList/TreeModel_stub.o gtk/general/Signal_stub.o	       \
        gtk/treeList/TreeSelection_stub.o gtk/glib/GObject_stub.o      \
        gtk/treeList/TreeView_stub.o:
	touch $(@:.o=.c)
	$(CC) -c -o $@ $(@:.o=.c)


am_libHSgtk_a_OBJECTS = \
  $(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtk_a_SOURCES))))

libHSgtk_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgtk_a_SOURCES))
libHSgtk_a_CHSFILES = \
  $(filter %.chs,$(libHSgtk_a_SOURCES:.chs.pp=.chs))
libHSgtk_a_CHSFILES_HS = $(libHSgtk_a_CHSFILES:.chs=.hs)
libHSgtk_a_HSCFILES = $(filter %.hsc, $(libHSgtk_a_SOURCES))
libHSgtk_a_HSCFILES_HS = $(libHSgtk_a_HSCFILES:.hsc=.hs)
libHSgtk_a_BUILDSOURCES = \
	$(libHSgtk_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtk_a_CHSFILES_HS) \
	$(libHSgtk_a_HSCFILES_HS)
libHSgtk_a_HSFILES  = \
	$(filter %.hs,$(libHSgtk_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtk_a_SOURCES))

# add prefix nobase_ when changing to hierarchical module namespace
hi_SCRIPTS = $(libHSgtk_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES+ = $(am_libHSgtk_a_OBJECTS)
MOSTLYCLEANFILES+ = $(libHSgtk_a_HSFILES:.hs=.hi)
CLEANFILES+= $(libHSgtk_a_BUILDSOURCES)

DISTCLEANFILES+= libHSgtk_a.deps $(libHSgtk_a_CHSFILES_HS:.hs=.dep)
ifneq ($(MAKECMDGOALS),clean)
-include libHSgtk_a.deps $(libHSgtk_a_CHSFILES_HS:.hs=.dep)
 endif

#
#	mogul package
#
################################################################################

libHSmogul_a_NAME = libHSmogul.a
$(libHSmogul_a_NAME) : NAME = libHSmogul_a

libHSmogul_a_PACKAGE		= mogul/mogul.pkg
libHSmogul_a_PACKAGEDEPS	= 
libHSmogul_a_LIBS		= 
libHSmogul_a_HCFLAGS		= 
libHSmogul_a_CFLAGS 		= 
libHSmogul_a_CPPFLAGS 		= $(CPPFLAGS)

libHSmogul_a_SOURCES =		\
	mogul/GetWidget.hs	\
	mogul/MDialog.hs	\
	mogul/Mogul.hs		\
	mogul/NewWidget.hs	\
	mogul/TreeList.hs	\
	mogul/WidgetTable.hs

libHSmogul_a_LIBADD = libHSgtk.a

am_libHSmogul_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSmogul_a_SOURCES))))

libHSmogul_a_HSFILES = $(libHSmogul_a_SOURCES)
.PRECIOUS: $(libHSmogul_a_HSFILES:.hs=hi)

MOSTLYCLEANFILES+ = $(am_libHSmogul_a_OBJECTS)
MOSTLYCLEANFILES+ = $(libHSmogul_a_HSFILES:.hs=.hi)

DISTCLEANFILES+= libHSmogul_a.deps
ifneq ($(MAKECMDGOALS),clean)
-include libHSmogul_a.deps
 endif

#
#	glade package
#
################################################################################

libHSglade_a_NAME = libHSglade.a
$(libHSglade_a_NAME) : NAME = libHSglade_a

libHSglade_a_PACKAGE		= glade/glade.pkg
libHSglade_a_PACKAGEDEPS	= 
libHSglade_a_HEADER 		= glade/glade.h
libHSglade_a_PRECOMP		= glade/glade.precomp
libHSglade_a_LIBS		= $(LIBGLADE_LIBS)
libHSglade_a_HCFLAGS		= -fglasgow-exts
libHSglade_a_CFLAGS		= $(filter-out -I% -D%,$(GTK_CFLAGS) $(LIBGLADE_CFLAGS))
libHSglade_a_CPPFLAGS		= $(filter -I% -D%,$(GTK_CFLAGS) $(LIBGLADE_CFLAGS))

libHSglade_a_SOURCES =	        \
	glade/GladeType.chs	\
        glade/Glade.chs

libHSglade_a_LIBADD = libHSgtk.a

glade/GladeType.chs : $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/TypeGenerator
		      $(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
		      $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $@ --tag=libglade --lib=glade --prefix=glade)

am_libHSglade_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSglade_a_SOURCES))))

libHSglade_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglade_a_SOURCES))
libHSglade_a_CHSFILES = \
  $(filter %.chs,$(libHSglade_a_SOURCES:.chs.pp=.chs))
libHSglade_a_CHSFILES_HS = $(libHSglade_a_CHSFILES:.chs=.hs)
libHSglade_a_HSCFILES = $(filter %.hsc, $(libHSglade_a_SOURCES))
libHSglade_a_HSCFILES_HS = $(libHSglade_a_HSCFILES:.hsc=.hs)
libHSglade_a_BUILDSOURCES = \
	$(libHSglade_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSglade_a_CHSFILES_HS) \
        glade/GladeType.chs
libHSglade_a_HSFILES  = \
	$(filter %.hs,$(libHSglade_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSglade_a_SOURCES))

MOSTLYCLEANFILES += $(am_libHSglade_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.hi)
CLEANFILES += $(libHSglade_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSglade_a.deps $(libHSglade_a_CHSFILES_HS:.hs=.dep)
ifneq ($(MAKECMDGOALS),clean)
-include libHSglade_a.deps $(libHSglade_a_CHSFILES:.chs=.dep)
 endif

#
#	gconf package
#
################################################################################

libHSgconf_a_NAME = libHSgconf.a
$(libHSgconf_a_NAME) : NAME = libHSgconf_a

libHSgconf_a_PACKAGE		= gconf/gconf.pkg
libHSgconf_a_PACKAGEDEPS	= 
libHSgconf_a_HEADER 		= gconf/gconf-client.h
libHSgconf_a_PRECOMP		= gconf/gconf.precomp
libHSgconf_a_LIBS		= $(GCONF_LIBS)
libHSgconf_a_HCFLAGS		= -fglasgow-exts -fallow-overlapping-instances
libHSgconf_a_CFLAGS		= $(filter-out -I% -D%,$(GTK_CFLAGS) $(GCONF_CFLAGS))
libHSgconf_a_CPPFLAGS		= $(filter -I% -D%,$(GTK_CFLAGS) $(GCONF_CFLAGS))

libHSgconf_a_SOURCES =	        			\
	gconf/System/Gnome/GConf/GConfType.chs		\
        gconf/System/Gnome/GConf/GConfValue.chs		\
        gconf/System/Gnome/GConf/GConfClient.chs	\
        gconf/System/Gnome/GConf.hs

libHSgconf_a_LIBADD = libHSgtk.a

gconf/System/Gnome/GConf/GConfType.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$@ --tag=gconf --lib=gconf --prefix=gconf \
	--modname=System.Gnome.GConf.GConfType)

am_libHSgconf_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgconf_a_SOURCES))))

libHSgconf_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgconf_a_SOURCES))
libHSgconf_a_CHSFILES = \
  $(filter %.chs,$(libHSgconf_a_SOURCES:.chs.pp=.chs))
libHSgconf_a_CHSFILES_HS = $(libHSgconf_a_CHSFILES:.chs=.hs)
libHSgconf_a_HSCFILES = $(filter %.hsc, $(libHSgconf_a_SOURCES))
libHSgconf_a_HSCFILES_HS = $(libHSgconf_a_HSCFILES:.hsc=.hs)
libHSgconf_a_BUILDSOURCES = \
	$(libHSgconf_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgconf_a_CHSFILES_HS) \
        gconf/System/Gnome/GConf/GConfType.chs
libHSgconf_a_HSFILES  = \
	$(filter %.hs,$(libHSgconf_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgconf_a_SOURCES))

MOSTLYCLEANFILES += $(am_libHSgconf_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.hi)
CLEANFILES += $(libHSgconf_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSgconf_a.deps $(libHSgconf_a_CHSFILES_HS:.hs=.dep)

ifneq ($(MAKECMDGOALS),clean)
-include libHSgconf_a.deps $(libHSgconf_a_CHSFILES:.chs=.dep)
 endif

#
#	sourceview package
#
################################################################################

libHSsourceview_a_NAME = libHSsourceview.a
$(libHSsourceview_a_NAME) : NAME = libHSsourceview_a

libHSsourceview_a_PACKAGE	= sourceview/sourceview.pkg
libHSsourceview_a_PACKAGEDEPS	= 
libHSsourceview_a_HEADER 	= sourceview.h
libHSsourceview_a_PRECOMP	= sourceview/sourceview.precomp
libHSsourceview_a_LIBS		= $(SOURCEVIEW_LIBS)
libHSsourceview_a_HCFLAGS	= -fglasgow-exts
libHSsourceview_a_CFLAGS	= $(filter-out -I% -D%,$(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS))
libHSsourceview_a_CPPFLAGS	= $(filter -I% -D%,$(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS)) -Isourceview

libHSsourceview_a_SOURCES =			\
	sourceview/SourceViewType.chs		\
	sourceview/SourceBuffer.chs		\
	sourceview/SourceIter.chs		\
	sourceview/SourceLanguage.chs		\
	sourceview/SourceLanguagesManager.chs	\
	sourceview/SourceMarker.chs		\
	sourceview/SourceStyleScheme.chs	\
	sourceview/SourceTag.chs		\
        sourceview/SourceTagStyle.hsc		\
	sourceview/SourceTagTable.chs		\
	sourceview/SourceView.chs

libHSsourceview_a_LIBADD = libHSgtk.a

sourceview/SourceViewType.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$@ --tag=sourceview)

am_libHSsourceview_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsourceview_a_SOURCES))))

libHSsourceview_a_CHSPPFILES = $(filter %.chs.pp,$(libHSsourceview_a_SOURCES))
libHSsourceview_a_CHSFILES = \
  $(filter %.chs,$(libHSsourceview_a_SOURCES:.chs.pp=.chs))
libHSsourceview_a_CHSFILES_HS = $(libHSsourceview_a_CHSFILES:.chs=.hs)
libHSsourceview_a_HSCFILES = $(filter %.hsc, $(libHSsourceview_a_SOURCES))
libHSsourceview_a_HSCFILES_HS = $(libHSsourceview_a_HSCFILES:.hsc=.hs)
libHSsourceview_a_BUILDSOURCES = \
	$(libHSsourceview_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSsourceview_a_CHSFILES_HS) \
        $(libHSsourceview_a_HSCFILES_HS) \
        sourceview/SourceViewType.chs
libHSsourceview_a_HSFILES  = \
	$(filter %.hs,$(libHSsourceview_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsourceview_a_SOURCES))

MOSTLYCLEANFILES += $(am_libHSsourceview_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.hi)
CLEANFILES += $(libHSsourceview_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSsourceview_a.deps $(libHSsourceview_a_CHSFILES_HS:.hs=.dep)

ifneq ($(MAKECMDGOALS),clean)
-include libHSsourceview_a.deps $(libHSsourceview_a_CHSFILES:.chs=.dep)
 endif


# All generated source files go here.
BUILDSOURCES =	$(libHSgtk_a_BUILDSOURCES)	\
		$(libHSglade_a_BUILDSOURCES)	\
		$(libHSgconf_a_BUILDSOURCES)	\
		$(libHSsourceview_a_BUILDSOURCES)

include mk/common.mk
