AUTOMAKE_OPTIONS = foreign subdir-objects
SUFFIXES = .chs.pp .chs .hsc .deps .dep
MOSTLYCLEANFILES = *.deps.bak
CLEANFILES =
DISTCLEANFILES = */*.precomp
#CPPFLAGS += $(filter -I% -D%, \
#	$(GLIB_CFLAGS) $(GTK_CFLAGS) \
#	$(SOURCEVIEW_CFLAGS) $(LIBGLADE_CFLAGS) \
#	$(GCONF_CFLAGS) $(MOZEMBED_CFLAGS))

# Build c2hs before anything else.

EXTRA_DIST = \
	tools/callbackGen/Signal.chs-boot1	\
	tools/callbackGen/Signal.chs-boot2	\
	tools/callbackGen/gtkmarshal.list	\
	tools/checkDirs.sh			\
	tools/c2hs/toplevel/c2hs_config.h	\
	tools/hierarchyGen/hierarchy.list	\
	tools/hierarchyGen/Hierarchy.chs.template

CHSDEPEND = $(srcdir)/mk/chsDepend

# all packages and applications
lib_LIBRARIES =	\
        libHSglib.a	\
	libHSgtk.a	\
        libHSmogul.a
if ENABLE_LIBGLADE
lib_LIBRARIES += libHSglade.a
endif
if ENABLE_GNOME
lib_LIBRARIES += 	\
        libHSgconf.a	\
        libHSsourceview.a
endif
if ENABLE_MOZEMBED
lib_LIBRARIES += libHSmozembed.a
endif

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
tools_hierarchyGen_TypeGenerator_SOURCESDIRS = tools/hierarchyGen
tools_hierarchyGen_TypeGenerator_SOURCES = \
  tools/hierarchyGen/TypeGen.hs
# Fix automake - the subdir-objects option doesn't work here.
am_tools_hierarchyGen_TypeGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_hierarchyGen_TypeGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_hierarchyGen_TypeGenerator_OBJECTS)
MOSTLYCLEANFILES+= $(tools_hierarchyGen_TypeGenerator_SOURCES:.hs=.hi)

#
#	HookGenerator Tool
#
################################################################################

HookGenerator_NAME = tools/callbackGen/HookGenerator
$(HookGenerator_NAME) : NAME = tools_callbackGen_HookGenerator

tools_callbackGen_HookGenerator_MAIN = \
  $(tools_callbackGen_HookGenerator_SOURCES)
tools_callbackGen_HookGenerator_PACKAGEDEPS = 
tools_callbackGen_HookGenerator_SOURCESDIRS = tools/callbackGen
tools_callbackGen_HookGenerator_SOURCES = \
  tools/callbackGen/HookGenerator.hs
am_tools_callbackGen_HookGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_callbackGen_HookGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_callbackGen_HookGenerator_OBJECTS)
MOSTLYCLEANFILES+= $(tools_callbackGen_HookGenerator_SOURCES:.hs=.hi)


gtk/Graphics/UI/Gtk/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs-boot1 \
	$(srcdir)/tools/callbackGen/Signal.chs-boot2 \
	$(srcdir)/tools/callbackGen/gtkmarshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator
	$(strip $(srcdir)/tools/callbackGen/HookGenerator $(MARSHALLDEFS) \
	$(srcdir)/tools/callbackGen/ $@ \
	$(if $(subst yes,,$(FOUR_WORD_CALLBACK)),--broken))

#
#	API Coverage Tool
#
################################################################################

Exclude_NAME = tools/apicoverage/Exclude
$(Exclude_NAME) : NAME = tools_apicoverage_Exclude

tools_apicoverage_Exclude_MAIN = \
  $(tools_apicoverage_Exclude_SOURCES)
tools_apicoverage_Exclude_PACKAGEDEPS = 
tools_apicoverage_Exclude_SOURCESDIRS = tools/apicoverage
tools_apicoverage_Exclude_SOURCES = \
  tools/apicoverage/Exclude.hs
am_tools_apicoverage_Exclude_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_apicoverage_Exclude_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_apicoverage_Exclude_OBJECTS)
MOSTLYCLEANFILES+= $(tools_apicoverage_Exclude_SOURCES:.hs=.hi)

#
#	c2hs interface generator
#
################################################################################

tools_c2hs_c2hsLocal_NAME = tools/c2hs/c2hsLocal
$(tools_c2hs_c2hsLocal_NAME) : NAME = tools_c2hs_c2hsLocal

tools_c2hs_c2hsLocal_MAIN = c2hs/toplevel/Main.hs
tools_c2hs_c2hsLocal_PACKAGEDEPS	= 
tools_c2hs_c2hsLocal_HEADER		= 
tools_c2hs_c2hsLocal_HCFLAGS		= 

tools_c2hs_c2hsLocal_SOURCESDIRS = 					     \
	tools/c2hs/base/admin tools/c2hs/base/errors			     \
	tools/c2hs/base/general tools/c2hs/base/graphs			     \
	tools/c2hs/base/state tools/c2hs/base/syms			     \
	tools/c2hs/base/syntax tools/c2hs/base/sysdep tools/c2hs/c	     \
	tools/c2hs/chs tools/c2hs/gen tools/c2hs/state tools/c2hs/toplevel

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

tools_c2hs_base_syntax_Parsers_hs_HCFLAGS = -fglasgow-exts
tools_c2hs_toplevel_C2HSConfig_hs_HCFLAGS = -fffi -fvia-C \
	'-\#include<tools/c2hs/toplevel/c2hs_config.h>'

tools_c2hs_c2hsLocal_HSFILES		= \
	$(filter %.hs,$(tools_c2hs_c2hsLocal_SOURCES))
# Fix automake - the subdir-objects option doesn't work here.
am_tools_c2hs_c2hsLocal_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(tools_c2hs_c2hsLocal_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_c2hs_c2hsLocal_OBJECTS)
MOSTLYCLEANFILES+= $(tools_c2hs_c2hsLocal_HSFILES:.hs=.hi)
CLEANFILES+= $(tools_c2hs_c2hsLocal_BUILDSOURCES)
DISTCLEANFILES+= tools_c2hs_c2hsLocal.deps
ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include tools_c2hs_c2hsLocal.deps
 endif

#
#	glib package
#
################################################################################

libHSglib_a_NAME = libHSglib.a
$(libHSglib_a_NAME) : NAME = libHSglib_a

libHSglib_a_PACKAGE		= glib/glib.pkg
libHSglib_a_PACKAGEDEPS		= 
libHSglib_a_HEADER 		= glib-object.h
libHSglib_a_PRECOMP		= glib/glib.precomp
libHSglib_a_LIBS		= $(GLIB_LIBS)
libHSglib_a_HCFLAGS		= -fffi
libHSglib_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS))
libHSglib_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS))

libHSglib_a_SOURCESDIRS = glib

libHSglib_a_SOURCES =	        		\
	glib/System/Glib.hs			\
	glib/System/Glib/FFI.hs			\
	glib/System/Glib/UTFString.hs		\
	glib/System/Glib/Types.chs		\
	glib/System/Glib/GType.chs		\
	glib/System/Glib/GValue.chs		\
	glib/System/Glib/GValueTypes.chs	\
	glib/System/Glib/GParameter.hsc		\
	glib/System/Glib/StoreValue.hsc		\
	glib/System/Glib/GObject.chs		\
	glib/System/Glib/GError.chs		\
	glib/System/Glib/GList.chs

glib_System_Glib_Types_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_GError_hs_HCFLAGS	= -fglasgow-exts

libHSglib_a_LIBADD =	\
	glib/System/Glib/GObject_stub.o

am_libHSglib_a_OBJECTS = $(libHSglib_a_HSFILES:.hs=.$(OBJEXT))

libHSglib_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglib_a_SOURCES))
libHSglib_a_CHSFILES = \
  $(filter %.chs,$(libHSglib_a_SOURCES:.chs.pp=.chs))
libHSglib_a_CHSFILES_HS = $(libHSglib_a_CHSFILES:.chs=.hs)
libHSglib_a_HSCFILES    = $(filter %.hsc, $(libHSglib_a_SOURCES))
libHSglib_a_HSCFILES_HS = $(libHSglib_a_HSCFILES:.hsc=.hs)
libHSglib_a_BUILDSOURCES = \
	$(libHSglib_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSglib_a_CHSFILES_HS) \
	$(libHSglib_a_HSCFILES_HS)
libHSglib_a_HSFILES  = \
	$(filter %.hs,$(libHSglib_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSglib_a_SOURCES))

nobase_hi_SCRIPTS = $(libHSglib_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSglib_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSglib_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSglib_a_CHSFILES:.chs=.chi)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.h)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.o)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.c)
CLEANFILES += $(libHSglib_a_BUILDSOURCES)

DISTCLEANFILES+= libHSglib_a.deps $(libHSglib_a_CHSFILES_HS:.hs=.dep)
ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include libHSglib_a.deps $(libHSglib_a_CHSFILES:.chs=.dep)
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
libHSgtk_a_LIBS			= $(GTK_LIBS)
libHSgtk_a_HCFLAGS		= -fffi
libHSgtk_a_CFLAGS 		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))
libHSgtk_a_CPPFLAGS 		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))

libHSgtk_a_SOURCESDIRS = $(libHSglib_a_SOURCESDIRS) gtk

libHSgtk_a_SOURCES =					 		\
	gtk/Graphics/UI/Gtk.hs						\
	gtk/Graphics/UI/Gtk/Types.chs				        \
	gtk/Graphics/UI/Gtk/Signals.chs				        \
	gtk/Graphics/UI/Gtk/Pango/Types.chs.pp				\
	gtk/Graphics/UI/Gtk/TreeList/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeViewColumn.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextIter.chs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Region.chs.pp				\
	gtk/Graphics/UI/Gtk/Abstract/Bin.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Box.chs				\
	gtk/Graphics/UI/Gtk/Abstract/ButtonBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Container.chs			\
	gtk/Graphics/UI/Gtk/Abstract/Misc.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Object.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Paned.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Range.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Scale.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Widget.chs				\
	gtk/Graphics/UI/Gtk/Buttons/Button.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/CheckButton.chs			\
	gtk/Graphics/UI/Gtk/Buttons/RadioButton.chs			\
	gtk/Graphics/UI/Gtk/Buttons/ToggleButton.chs			\
	gtk/Graphics/UI/Gtk/Display/AccelLabel.chs			\
	gtk/Graphics/UI/Gtk/Display/Image.chs				\
	gtk/Graphics/UI/Gtk/Display/Label.chs				\
	gtk/Graphics/UI/Gtk/Display/ProgressBar.chs			\
	gtk/Graphics/UI/Gtk/Display/Statusbar.chs			\
	gtk/Graphics/UI/Gtk/Entry/Editable.chs				\
	gtk/Graphics/UI/Gtk/Entry/Entry.chs.pp				\
	gtk/Graphics/UI/Gtk/Entry/EntryCompletion.chs.pp		\
	gtk/Graphics/UI/Gtk/Entry/HScale.chs				\
	gtk/Graphics/UI/Gtk/Entry/SpinButton.chs			\
	gtk/Graphics/UI/Gtk/Entry/VScale.chs				\
	gtk/Graphics/UI/Gtk/General/Enums.chs.pp			\
	gtk/Graphics/UI/Gtk/General/General.chs				\
	gtk/Graphics/UI/Gtk/General/IconFactory.chs.pp			\
	gtk/Graphics/UI/Gtk/General/Style.chs				\
	gtk/Graphics/UI/Gtk/Layout/Alignment.chs.pp			\
	gtk/Graphics/UI/Gtk/Layout/AspectFrame.chs			\
	gtk/Graphics/UI/Gtk/Layout/Expander.chs.pp			\
	gtk/Graphics/UI/Gtk/Layout/Fixed.chs				\
	gtk/Graphics/UI/Gtk/Layout/HBox.chs				\
	gtk/Graphics/UI/Gtk/Layout/HButtonBox.chs			\
	gtk/Graphics/UI/Gtk/Layout/HPaned.chs				\
	gtk/Graphics/UI/Gtk/Layout/Layout.chs				\
	gtk/Graphics/UI/Gtk/Layout/Notebook.chs.pp			\
	gtk/Graphics/UI/Gtk/Layout/Table.chs				\
	gtk/Graphics/UI/Gtk/Layout/VBox.chs				\
	gtk/Graphics/UI/Gtk/Layout/VButtonBox.chs			\
	gtk/Graphics/UI/Gtk/Layout/VPaned.chs				\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Combo.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ComboBox.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ComboBoxEntry.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Menu.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuBar.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuShell.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/TearoffMenuItem.chs	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Toolbar.chs.pp		\
	gtk/Graphics/UI/Gtk/Misc/Adjustment.chs				\
	gtk/Graphics/UI/Gtk/Misc/Calendar.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/DrawingArea.chs			\
	gtk/Graphics/UI/Gtk/Misc/EventBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/GArrow.chs				\
	gtk/Graphics/UI/Gtk/Misc/HandleBox.chs				\
	gtk/Graphics/UI/Gtk/Misc/SizeGroup.chs				\
	gtk/Graphics/UI/Gtk/Misc/Tooltips.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Viewport.chs				\
	gtk/Graphics/UI/Gtk/Multiline/TextBuffer.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextMark.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextTag.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextTagTable.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextView.chs			\
	gtk/Graphics/UI/Gtk/Ornaments/Frame.chs				\
	gtk/Graphics/UI/Gtk/Ornaments/HSeparator.chs			\
	gtk/Graphics/UI/Gtk/Ornaments/VSeparator.chs			\
	gtk/Graphics/UI/Gtk/Scrolling/HScrollbar.chs			\
	gtk/Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs		\
	gtk/Graphics/UI/Gtk/Scrolling/VScrollbar.chs			\
	gtk/Graphics/UI/Gtk/Selectors/ColorSelection.chs		\
	gtk/Graphics/UI/Gtk/Selectors/ColorSelectionDialog.chs		\
	gtk/Graphics/UI/Gtk/Selectors/FontSelection.chs			\
	gtk/Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooser.chs			\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserWidget.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserDialog.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererPixbuf.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererText.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererToggle.chs		\
	gtk/Graphics/UI/Gtk/TreeList/ListStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModelSort.chs			\
	gtk/Graphics/UI/Gtk/TreeList/TreeSelection.chs			\
	gtk/Graphics/UI/Gtk/TreeList/TreeStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/Dialog.chs				\
	gtk/Graphics/UI/Gtk/Windows/FileSel.chs				\
	gtk/Graphics/UI/Gtk/Windows/Window.chs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Drawable.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/GC.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Gdk.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Enums.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Keys.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Pixbuf.chs				\
	gtk/Graphics/UI/Gtk/Pango/Description.chs			\
	gtk/Graphics/UI/Gtk/Pango/Enums.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Layout.chs				\
	gtk/Graphics/UI/Gtk/Pango/Rendering.chs.pp			\
	gtk/Graphics/UI/Gtk/Embedding/Plug.chs				\
	gtk/Graphics/UI/Gtk/Embedding/Socket.chs			\
	gtk/Graphics/UI/Gtk/General/StockItems.hsc			\
	gtk/Graphics/UI/Gtk/General/Structs.hsc				\
	gtk/Graphics/UI/Gtk/Gdk/Events.hsc				\
	gtk/Graphics/UI/Gtk/Embedding/Embedding.hsc			\
	gtk/Graphics/UI/Gtk/Abstract/Scrollbar.hs			\
	gtk/Graphics/UI/Gtk/Abstract/Separator.hs			\
	gtk/Graphics/UI/Gtk/TreeList/CellRenderer.hs			\
	gtk/Graphics/UI/Gtk/Gdk/DrawWindow.hs				\
	gtk/Graphics/UI/Gtk/Pango/Markup.hs

gtk_Graphics_UI_Gtk_hs_HCFLAGS			= -fno-warn-duplicate-exports	#FIXME
gtk_Graphics_UI_Gtk_Gdk_Enums_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Types_hs_HCFLAGS	        = -fglasgow-exts
gtk_Graphics_UI_Gtk_General_Structs_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Events_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_General_StockItems_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Region_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Drawable_hs_HCFLAGS	= -fglasgow-exts

libHSgtk_a_LIBADD =							\
	gtk/Graphics/UI/Gtk/Abstract/Container_stub.o			\
	gtk/Graphics/UI/Gtk/Multiline/TextIter_stub.o			\
	gtk/Graphics/UI/Gtk/Entry/EntryCompletion_stub.o		\
        gtk/Graphics/UI/Gtk/Multiline/TextTagTable_stub.o		\
	gtk/Graphics/UI/Gtk/General/General_stub.o			\
        gtk/Graphics/UI/Gtk/TreeList/TreeModel_stub.o			\
	gtk/Graphics/UI/Gtk/Signals_stub.o	        		\
        gtk/Graphics/UI/Gtk/TreeList/TreeSelection_stub.o		\
        gtk/Graphics/UI/Gtk/TreeList/TreeView_stub.o

# Pretend these stub files can be created by a simple rule.
$(libHSgtk_a_LIBADD) :
	touch $(@:.o=.c)
	$(CC) -c -o $@ $(@:.o=.c)

gtk/Graphics/UI/Gtk/Types.chs : \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
        $(srcdir)/tools/hierarchyGen/hierarchy.list \
        $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
        $@ $(addprefix --tag=,$(CREATE_TYPES)) \
	--modname=Graphics.UI.Gtk.Types \
        --parentname=System.Glib.GObject)

am_libHSgtk_a_OBJECTS = \
  $(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtk_a_SOURCES))))

libHSgtk_a_CHSPPFILES   = $(filter %.chs.pp,$(libHSgtk_a_SOURCES))
libHSgtk_a_CHSFILES     = $(filter %.chs,$(libHSgtk_a_SOURCES:.chs.pp=.chs))
libHSgtk_a_CHSFILES_HS  = $(libHSgtk_a_CHSFILES:.chs=.hs)
libHSgtk_a_HSCFILES     = $(filter %.hsc, $(libHSgtk_a_SOURCES))
libHSgtk_a_HSCFILES_HS  = $(libHSgtk_a_HSCFILES:.hsc=.hs)

libHSgtk_a_BUILDSOURCES = \
	$(libHSgtk_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtk_a_CHSFILES_HS) \
	$(libHSgtk_a_HSCFILES_HS)
libHSgtk_a_HSFILES  = \
	$(filter %.hs,$(libHSgtk_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtk_a_SOURCES))

nobase_hi_SCRIPTS += $(libHSgtk_a_HSFILES:.hs=.hi)

if BUILDDOCS
htmldir = $(prefix)/html
html_DATA = $(foreach HSFILE, $(libHSgtk_a_HSFILES), \
		$(patsubst gtk.%.hs, doc/%.html, $(subst /,.,$(HSFILE))))
EXCLUDE_DOCS = 
PREPROC_DOCS = gtk/Graphics/UI/Gtk.hs

$(html_DATA) : $(libHSgtk_a_HSFILES) $(PREPROC_DOCS:.hs=.hs.uncpp)
	mkdir -p doc
	$(HADDOCK) --html -o doc \
		--title="Gtk2Hs" --package=$(libHSgtk_a_NAME) \
		--dump-interface=doc/gtk.haddock \
		$(filter-out $(EXCLUDE_DOCS) $(PREPROC_DOCS), $^)

.hs.hs.uncpp : $(CONFIG_H)
	$(HSCPP) -I. $< -o $@

MOSTLYCLEANFILES += $(html_DATA)

endif #BUILDDOCS

MOSTLYCLEANFILES += $(am_libHSgtk_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgtk_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSgtk_a_CHSFILES:.chs=.chi)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.c)
CLEANFILES+= $(libHSgtk_a_BUILDSOURCES)

DISTCLEANFILES+= libHSgtk_a.deps $(libHSgtk_a_CHSFILES_HS:.hs=.dep)
ifeq (,$(findstring clean, $(MAKECMDGOALS)))
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
libHSmogul_a_CPPFLAGS 		= 

libHSmogul_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) mogul

libHSmogul_a_SOURCES =		\
	mogul/Graphics/UI/Gtk/Mogul.hs			\
	mogul/Graphics/UI/Gtk/Mogul/GetWidget.hs	\
	mogul/Graphics/UI/Gtk/Mogul/MDialog.hs		\
	mogul/Graphics/UI/Gtk/Mogul/NewWidget.hs	\
	mogul/Graphics/UI/Gtk/Mogul/TreeList.hs		\
	mogul/Graphics/UI/Gtk/Mogul/WidgetTable.hs

am_libHSmogul_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSmogul_a_SOURCES))))

libHSmogul_a_HSFILES = $(libHSmogul_a_SOURCES)
.PRECIOUS: $(libHSmogul_a_HSFILES:.hs=hi)

nobase_hi_SCRIPTS += $(libHSmogul_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSmogul_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSmogul_a_HSFILES:.hs=.hi)

DISTCLEANFILES+= libHSmogul_a.deps
ifeq (,$(findstring clean, $(MAKECMDGOALS)))
-include libHSmogul_a.deps
 endif

#
#	glade package
#
################################################################################

if ENABLE_LIBGLADE

libHSglade_a_NAME = libHSglade.a
$(libHSglade_a_NAME) : NAME = libHSglade_a

libHSglade_a_PACKAGE		= glade/glade.pkg
libHSglade_a_PACKAGEDEPS	= 
libHSglade_a_HEADER 		= glade/glade.h
libHSglade_a_PRECOMP		= glade/glade.precomp
libHSglade_a_LIBS		= $(LIBGLADE_LIBS)
libHSglade_a_HCFLAGS		= -fffi
libHSglade_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))
libHSglade_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))

libHSglade_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) glade

libHSglade_a_SOURCES =	        \
	glade/Graphics/UI/Gtk/Glade/Types.chs	\
        glade/Graphics/UI/Gtk/Glade.chs

glade_Graphics_UI_Gtk_Glade_Types_hs_HCFLAGS	= -fglasgow-exts

glade/Graphics/UI/Gtk/Glade/Types.chs : $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/TypeGenerator  \
		      $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
		      $(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
		      $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
		      $@ --tag=libglade --lib=glade --prefix=glade \
                      --modname=Graphics.UI.Gtk.Glade.Types \
		      --parentname=Graphics.UI.Gtk.Types)

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
        $(libHSglade_a_HSCFILES_HS)
libHSglade_a_HSFILES  = \
	$(filter %.hs,$(libHSglade_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSglade_a_SOURCES))

nobase_hi_SCRIPTS += $(libHSglade_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSglade_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSglade_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSglade_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSglade_a.deps $(libHSglade_a_CHSFILES_HS:.hs=.dep)
ifeq (,$(findstring clean, $(MAKECMDGOALS)))
-include libHSglade_a.deps $(libHSglade_a_CHSFILES:.chs=.dep)
 endif

endif #ENABLE_LIBGLADE

#
#	gconf package
#
################################################################################

if ENABLE_GNOME

libHSgconf_a_NAME = libHSgconf.a
$(libHSgconf_a_NAME) : NAME = libHSgconf_a

libHSgconf_a_PACKAGE		= gconf/gconf.pkg
libHSgconf_a_PACKAGEDEPS	= 
libHSgconf_a_HEADER 		= gconf/gconf-client.h
libHSgconf_a_PRECOMP		= gconf/gconf.precomp
libHSgconf_a_LIBS		= $(GCONF_LIBS)
libHSgconf_a_HCFLAGS		= -fglasgow-exts -fallow-overlapping-instances
libHSgconf_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GCONF_CFLAGS))
libHSgconf_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GCONF_CFLAGS))

libHSgconf_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) gconf

libHSgconf_a_SOURCES =	        			\
	gconf/System/Gnome/GConf/GConfType.chs		\
        gconf/System/Gnome/GConf/GConfValue.chs		\
        gconf/System/Gnome/GConf/GConfClient.chs	\
        gconf/System/Gnome/GConf.hs

gconf/System/Gnome/GConf/GConfType.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gconf --lib=gconf --prefix=gconf \
	--modname=System.Gnome.GConf.GConfType --parentname=System.Glib.GObject)

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
        $(libHSgconf_a_HSCFILES_HS) \
        gconf/System/Gnome/GConf/GConfType.chs
libHSgconf_a_HSFILES  = \
	$(filter %.hs,$(libHSgconf_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgconf_a_SOURCES))

nobase_hi_SCRIPTS += $(libHSgconf_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSgconf_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSgconf_a_CHSFILES:.chs=.chi)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.c)
CLEANFILES += $(libHSgconf_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSgconf_a.deps $(libHSgconf_a_CHSFILES_HS:.hs=.dep)

ifeq (,$(findstring clean, $(MAKECMDGOALS)))
-include libHSgconf_a.deps $(libHSgconf_a_CHSFILES:.chs=.dep)
 endif

endif #ENABLE_GNOME

#
#	sourceview package
#
################################################################################

if ENABLE_GNOME

libHSsourceview_a_NAME = libHSsourceview.a
$(libHSsourceview_a_NAME) : NAME = libHSsourceview_a

libHSsourceview_a_PACKAGE	= sourceview/sourceview.pkg
libHSsourceview_a_PACKAGEDEPS	= 
libHSsourceview_a_HEADER 	= sourceview.h
libHSsourceview_a_PRECOMP	= sourceview/sourceview.precomp
libHSsourceview_a_LIBS		= $(SOURCEVIEW_LIBS)
libHSsourceview_a_HCFLAGS	= -fffi
libHSsourceview_a_CFLAGS	= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS))
libHSsourceview_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS)) -Isourceview

libHSsourceview_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) sourceview

libHSsourceview_a_SOURCES =			\
	sourceview/Graphics/UI/Gtk/SourceView.hs				\
	sourceview/Graphics/UI/Gtk/SourceView/Types.chs				\
	sourceview/Graphics/UI/Gtk/SourceView/SourceBuffer.chs			\
	sourceview/Graphics/UI/Gtk/SourceView/SourceIter.chs			\
	sourceview/Graphics/UI/Gtk/SourceView/SourceLanguage.chs		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceLanguagesManager.chs	\
	sourceview/Graphics/UI/Gtk/SourceView/SourceMarker.chs			\
	sourceview/Graphics/UI/Gtk/SourceView/SourceStyleScheme.chs		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceTag.chs			\
        sourceview/Graphics/UI/Gtk/SourceView/SourceTagStyle.hsc		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceTagTable.chs		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceView.chs

sourceview_Graphics_UI_Gtk_SourceView_Types_hs_HCFLAGS	= -fglasgow-exts
sourceview_Graphics_UI_Gtk_SourceView_SourceTagStyle_hs_HCFLAGS	= -fglasgow-exts

sourceview/Graphics/UI/Gtk/SourceView/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=sourceview --parentname=Graphics.UI.Gtk.Types \
        --modname=Graphics.UI.Gtk.SourceView.Types)

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
        $(libHSsourceview_a_HSCFILES_HS)
libHSsourceview_a_HSFILES  = \
	$(filter %.hs,$(libHSsourceview_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsourceview_a_SOURCES))

nobase_hi_SCRIPTS += $(libHSsourceview_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSsourceview_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSsourceview_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSsourceview_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSsourceview_a.deps $(libHSsourceview_a_CHSFILES_HS:.hs=.dep)

ifeq (,$(findstring clean, $(MAKECMDGOALS)))
-include libHSsourceview_a.deps $(libHSsourceview_a_CHSFILES:.chs=.dep)
 endif

endif #ENABLE_GNOME

#
#	mozembed package
#
################################################################################

if ENABLE_MOZEMBED

libHSmozembed_a_NAME = libHSmozembed.a
$(libHSmozembed_a_NAME) : NAME = libHSmozembed_a

libHSmozembed_a_PACKAGE		= mozembed/mozembed.pkg
libHSmozembed_a_PACKAGEDEPS	= 
libHSmozembed_a_HEADER 		= gtkmozembed.h
libHSmozembed_a_PRECOMP		= mozembed/mozembed.precomp
libHSmozembed_a_LIBS		= $(MOZEMBED_LIBS)
libHSmozembed_a_HCFLAGS		= -fffi
libHSmozembed_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))
libHSmozembed_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))

libHSmozembed_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) mozembed

libHSmozembed_a_SOURCES =				\
	mozembed/Graphics/UI/Gtk/MozEmbedType.chs	\
	mozembed/Graphics/UI/Gtk/MozEmbed.chs

mozembed_Graphics_UI_Gtk_MozEmbedType_hs_HCFLAGS = -fglasgow-exts

mozembed/Graphics/UI/Gtk/MozEmbedType.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=mozembed --parentname=Graphics.UI.Gtk.Types \
	--modname=Graphics.UI.Gtk.MozEmbedType)

am_libHSmozembed_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSmozembed_a_SOURCES))))

libHSmozembed_a_CHSPPFILES = $(filter %.chs.pp,$(libHSmozembed_a_SOURCES))
libHSmozembed_a_CHSFILES = \
  $(filter %.chs,$(libHSmozembed_a_SOURCES:.chs.pp=.chs))
libHSmozembed_a_CHSFILES_HS = $(libHSmozembed_a_CHSFILES:.chs=.hs)
libHSmozembed_a_HSCFILES = $(filter %.hsc, $(libHSmozembed_a_SOURCES))
libHSmozembed_a_HSCFILES_HS = $(libHSmozembed_a_HSCFILES:.hsc=.hs)
libHSmozembed_a_BUILDSOURCES = \
	$(libHSmozembed_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSmozembed_a_CHSFILES_HS) \
        $(libHSmozembed_a_HSCFILES_HS) \
        mozembed/Graphics/UI/Gtk/MozEmbedType.chs
libHSmozembed_a_HSFILES  = \
	$(filter %.hs,$(libHSmozembed_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSmozembed_a_SOURCES))

nobase_hi_SCRIPTS += $(libHSmozembed_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSmozembed_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSmozembed_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSmozembed_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSmozembed_a_BUILDSOURCES) 

DISTCLEANFILES+= libHSmozembed_a.deps $(libHSmozembed_a_CHSFILES_HS:.hs=.dep)

ifeq (,$(findstring clean, $(MAKECMDGOALS)))
-include libHSmozembed_a.deps $(libHSmozembed_a_CHSFILES:.chs=.dep)
 endif

endif #ENABLE_MOZEMBED

include mk/common.mk
