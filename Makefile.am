AUTOMAKE_OPTIONS = foreign subdir-objects
SUFFIXES = .hs.uncpp .chs.pp .chs .hsc .deps .dep
MOSTLYCLEANFILES = */*.deps.bak tools/*/*.deps.bak
CLEANFILES =
DISTCLEANFILES = */*.precomp

EXTRA_DIST = \
	tools/callbackGen/Signal.chs-boot1	\
	tools/callbackGen/Signal.chs-boot2	\
	tools/callbackGen/gtkmarshal.list	\
	tools/checkDirs.sh			\
	tools/c2hs/toplevel/c2hs_config.h	\
	tools/hierarchyGen/hierarchy.list	\
	tools/hierarchyGen/Hierarchy.chs.template \
	sourceview/sourceview.h			\
	gtk/wingtk.h

# There are no other files in these dirs and tar excludes empty dirs
dist-hook:
	mkdir $(distdir)/glade/Graphics/UI/Gtk/Glade
	touch $(distdir)/glade/Graphics/UI/Gtk/Glade/.keep
	mkdir $(distdir)/mozembed/Graphics/UI/Gtk/MozEmbed
	touch $(distdir)/mozembed/Graphics/UI/Gtk/MozEmbed/.keep

CHSDEPEND = $(srcdir)/mk/chsDepend

# all packages and applications
pkglib_LIBRARIES =	\
        libHSglib.a	\
	libHSgtk.a	\
        libHSmogul.a
if ENABLE_LIBGLADE
pkglib_LIBRARIES += libHSglade.a
endif
if ENABLE_GNOME
pkglib_LIBRARIES += 	\
        libHSgconf.a	\
        libHSsourceview.a
endif
if ENABLE_MOZEMBED
pkglib_LIBRARIES += libHSmozembed.a
endif

noinst_PROGRAMS = \
	tools/hierarchyGen/TypeGenerator \
	tools/callbackGen/HookGenerator \
	tools/c2hs/c2hsLocal

#
#	TypeGenerator Tool
#
################################################################################

# While building the program <name>, set the variable NAME to <name> so
# we can access program-specific variables like <name>_PACKAGEDEPS. The
# following is a hack to prevent automake from assuming that we are overriding
# the tools/hierarchyGen/TypeGenerator goal.
tools_hierarchyGen_PKGNAME = tools_hierarchyGen_TypeGenerator

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

tools_callbackGen_PKGNAME = tools_callbackGen_HookGenerator

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
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) $(MARSHALLDEFS) \
	$(srcdir)/tools/callbackGen/ $@ \
	$(if $(subst yes,,$(FOUR_WORD_CALLBACK)),--broken))

#
#	API Coverage Tool
#
################################################################################

#tools_apicoverage_PKGNAME = tools_apicoverage_Exclude
#
#tools_apicoverage_Exclude_MAIN = \
#  $(tools_apicoverage_Exclude_SOURCES)
#tools_apicoverage_Exclude_PACKAGEDEPS = 
#tools_apicoverage_Exclude_SOURCESDIRS = tools/apicoverage
#tools_apicoverage_Exclude_SOURCES = \
#  tools/apicoverage/Exclude.hs
#am_tools_apicoverage_Exclude_OBJECTS = $(addsuffix .$(OBJEXT),\
#  $(basename $(tools_apicoverage_Exclude_SOURCES)))
#MOSTLYCLEANFILES+= $(am_tools_apicoverage_Exclude_OBJECTS)
#MOSTLYCLEANFILES+= $(tools_apicoverage_Exclude_SOURCES:.hs=.hi)

#
#	c2hs interface generator
#
################################################################################

tools_c2hs_PKGNAME = tools_c2hs_c2hsLocal

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
	tools/c2hs/toplevel/c2hs_config.c

nodist_tools_c2hs_c2hsLocal_SOURCES =		\
	tools/c2hs/toplevel/C2HSConfig.hs

tools_c2hs_c2hsLocal_ALLSOURCES =		\
	$(tools_c2hs_c2hsLocal_SOURCES)		\
	$(nodist_tools_c2hs_c2hsLocal_SOURCES)

tools_c2hs_base_general_Binary_hs_HCFLAGS = -O -funbox-strict-fields
tools_c2hs_base_general_FastMutInt_hs_HCFLAGS = -O
tools_c2hs_base_syntax_Parsers_hs_HCFLAGS = -fglasgow-exts
tools_c2hs_toplevel_C2HSConfig_hs_HCFLAGS = -fffi -fvia-C \
	'-\#include<c2hs_config.h>'

tools_c2hs_c2hsLocal_HSFILES		= \
	$(filter %.hs,$(tools_c2hs_c2hsLocal_ALLSOURCES))
# Fix automake - the subdir-objects option doesn't work here.
am_tools_c2hs_c2hsLocal_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(tools_c2hs_c2hsLocal_ALLSOURCES)))
MOSTLYCLEANFILES+= $(am_tools_c2hs_c2hsLocal_OBJECTS)
MOSTLYCLEANFILES+= $(tools_c2hs_c2hsLocal_HSFILES:.hs=.hi)
CLEANFILES+= $(tools_c2hs_c2hsLocal_BUILDSOURCES)
DISTCLEANFILES+= tools/c2hs/c2hsLocal.deps
-include tools/c2hs/c2hsLocal.deps


#
#	glib package
#
################################################################################

glib_PKGNAME = libHSglib_a

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
	glib/System/Glib/GObject.chs.pp		\
	glib/System/Glib/GError.chs.pp		\
	glib/System/Glib/GList.chs

html_HSFILES_EXCLUDE = glib/System/Glib/FFI.hs

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

DISTCLEANFILES+= glib/libHSglib_a.deps $(libHSglib_a_CHSFILES_HS:.hs=.dep)

-include $(libHSglib_a_CHSFILES:.chs=.dep) glib/libHSglib_a.deps

#
#	gtk package
#
################################################################################

gtk_PKGNAME = libHSgtk_a

libHSgtk_a_PACKAGE		= gtk/gtk.pkg
libHSgtk_a_PACKAGEDEPS		= 
libHSgtk_a_HEADER 		= gtk/gtk.h
libHSgtk_a_PRECOMP		= gtk/gtk.precomp
libHSgtk_a_LIBS			= $(GLIB_LIBS) $(GTK_LIBS)
libHSgtk_a_HCFLAGS		= -fffi
libHSgtk_a_CFLAGS 		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))
libHSgtk_a_CPPFLAGS 		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))

libHSgtk_a_SOURCESDIRS = $(libHSglib_a_SOURCESDIRS) gtk
gtk/libHSgtk_a.deps	: glib/libHSglib_a.deps

libHSgtk_a_GENERATEDSOURCES =						\
	gtk/Graphics/UI/Gtk/Types.chs					\
	gtk/Graphics/UI/Gtk/Signals.chs

nodist_libHSgtk_a_SOURCES = $(libHSgtk_a_GENERATEDSOURCES)

libHSgtk_a_SOURCES =					 		\
	gtk/Graphics/UI/Gtk.hs						\
	gtk/Graphics/UI/Gtk/Pango/Types.chs.pp				\
	gtk/Graphics/UI/Gtk/TreeList/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeViewColumn.chs.pp		\
	gtk/Graphics/UI/Gtk/Multiline/TextIter.chs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Region.chs.pp				\
	gtk/Graphics/UI/Gtk/Abstract/Bin.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Box.chs				\
	gtk/Graphics/UI/Gtk/Abstract/ButtonBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Container.chs			\
	gtk/Graphics/UI/Gtk/Abstract/Misc.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Object.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Paned.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Range.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Scale.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Widget.chs				\
	gtk/Graphics/UI/Gtk/Buttons/Button.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/CheckButton.chs			\
	gtk/Graphics/UI/Gtk/Buttons/RadioButton.chs			\
	gtk/Graphics/UI/Gtk/Buttons/ToggleButton.chs			\
	gtk/Graphics/UI/Gtk/Display/AccelLabel.chs			\
	gtk/Graphics/UI/Gtk/Display/Image.chs.pp			\
	gtk/Graphics/UI/Gtk/Display/Label.chs				\
	gtk/Graphics/UI/Gtk/Display/ProgressBar.chs			\
	gtk/Graphics/UI/Gtk/Display/Statusbar.chs			\
	gtk/Graphics/UI/Gtk/Entry/Editable.chs.pp			\
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
	gtk/Graphics/UI/Gtk/Misc/Arrow.chs				\
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
	gtk/Graphics/UI/Gtk/Selectors/FileChooser.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserWidget.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserDialog.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererPixbuf.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererText.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererToggle.chs		\
	gtk/Graphics/UI/Gtk/TreeList/ListStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModelSort.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/Dialog.chs				\
	gtk/Graphics/UI/Gtk/Selectors/FileSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/Windows/Window.chs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Drawable.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/GC.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Gdk.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Enums.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Keys.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Pixbuf.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Description.chs			\
	gtk/Graphics/UI/Gtk/Pango/Enums.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Layout.chs				\
	gtk/Graphics/UI/Gtk/Pango/Rendering.chs.pp			\
	gtk/Graphics/UI/Gtk/General/StockItems.hsc			\
	gtk/Graphics/UI/Gtk/General/Structs.hsc				\
	gtk/Graphics/UI/Gtk/Gdk/Events.hsc				\
	gtk/Graphics/UI/Gtk/Abstract/Scrollbar.hs			\
	gtk/Graphics/UI/Gtk/Abstract/Separator.hs			\
	gtk/Graphics/UI/Gtk/TreeList/CellRenderer.hs			\
	gtk/Graphics/UI/Gtk/Gdk/DrawWindow.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Markup.hs

if !WIN32
libHSgtk_a_SOURCES +=							\
	gtk/Graphics/UI/Gtk/Embedding/Plug.chs				\
	gtk/Graphics/UI/Gtk/Embedding/Socket.chs			\
	gtk/Graphics/UI/Gtk/Embedding/Embedding.hsc
endif

EXTRA_libHSgtk_a_SOURCES =						\
	gtk/Graphics/UI/Gtk/Embedding/Plug.chs				\
	gtk/Graphics/UI/Gtk/Embedding/Socket.chs			\
	gtk/Graphics/UI/Gtk/Embedding/Embedding.hsc

html_HSFILES_PREPROC = gtk/Graphics/UI/Gtk.hs
html_HSFILES_HIDDEN  = $(libHSgtk_a_GENERATEDSOURCES:.chs=.hs)

gtk_Graphics_UI_Gtk_hs_HCFLAGS			= -fno-warn-duplicate-exports	#FIXME
gtk_Graphics_UI_Gtk_Gdk_Enums_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Types_hs_HCFLAGS	        = -fglasgow-exts
gtk_Graphics_UI_Gtk_General_Structs_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Events_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_General_StockItems_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Region_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Drawable_hs_HCFLAGS	= -fglasgow-exts

if WIN32
libHSgtk_a_HCFLAGS				+= -DWIN32
gtk_Graphics_UI_Gtk_General_General_hs_HCFLAGS	= '-\#include"gtk/wingtk.h"'
endif

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
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
        $(srcdir)/tools/hierarchyGen/hierarchy.list \
        $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
        $@ $(addprefix --tag=,$(CREATE_TYPES)) \
	--modname=Graphics.UI.Gtk.Types \
        --parentname=System.Glib.GObject)

libHSgtk_a_ALLSOURCES = $(libHSgtk_a_SOURCES) $(nodist_libHSgtk_a_SOURCES)

am_libHSgtk_a_OBJECTS = \
  $(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtk_a_ALLSOURCES))))

libHSgtk_a_CHSPPFILES   = $(filter %.chs.pp,$(libHSgtk_a_ALLSOURCES))
libHSgtk_a_CHSFILES     = $(filter %.chs,$(libHSgtk_a_ALLSOURCES:.chs.pp=.chs))
libHSgtk_a_CHSFILES_HS  = $(libHSgtk_a_CHSFILES:.chs=.hs)
libHSgtk_a_HSCFILES     = $(filter %.hsc, $(libHSgtk_a_ALLSOURCES))
libHSgtk_a_HSCFILES_HS  = $(libHSgtk_a_HSCFILES:.hsc=.hs)

libHSgtk_a_BUILDSOURCES = \
	$(libHSgtk_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtk_a_CHSFILES_HS) \
	$(libHSgtk_a_HSCFILES_HS) \
	$(libHSgtk_a_GENERATEDSOURCES)
libHSgtk_a_HSFILES  = \
	$(filter %.hs,$(libHSgtk_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtk_a_ALLSOURCES))

nobase_hi_SCRIPTS += $(libHSgtk_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSgtk_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgtk_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSgtk_a_CHSFILES:.chs=.chi)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.c)
CLEANFILES+= $(libHSgtk_a_BUILDSOURCES)

DISTCLEANFILES+= gtk/libHSgtk_a.deps $(libHSgtk_a_CHSFILES_HS:.hs=.dep)

$(libHSgtk_a_CHSFILES:.chs=.dep) : $(libHSgtk_a_GENERATEDSOURCES)
-include $(libHSgtk_a_CHSFILES_HS:.hs=.dep) gtk/libHSgtk_a.deps

#
#	mogul package
#
################################################################################

mogul_PKGNAME = libHSmogul_a

libHSmogul_a_PACKAGE		= mogul/mogul.pkg
libHSmogul_a_PACKAGEDEPS	= 
libHSmogul_a_LIBS		= 
libHSmogul_a_HCFLAGS		= 
libHSmogul_a_CFLAGS 		= 
libHSmogul_a_CPPFLAGS 		= 

libHSmogul_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) mogul
mogul/libHSmogul_a.deps	: gtk/libHSgtk_a.deps

libHSmogul_a_SOURCES =		\
	mogul/Graphics/UI/Gtk/Mogul.hs			\
	mogul/Graphics/UI/Gtk/Mogul/GetWidget.hs	\
	mogul/Graphics/UI/Gtk/Mogul/MDialog.hs		\
	mogul/Graphics/UI/Gtk/Mogul/NewWidget.hs	\
	mogul/Graphics/UI/Gtk/Mogul/TreeList.hs		\
	mogul/Graphics/UI/Gtk/Mogul/WidgetTable.hs

html_HSFILES_PREPROC += 	\
	mogul/Graphics/UI/Gtk/Mogul.hs			\
	mogul/Graphics/UI/Gtk/Mogul/GetWidget.hs	\
	mogul/Graphics/UI/Gtk/Mogul/NewWidget.hs

am_libHSmogul_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSmogul_a_SOURCES))))

libHSmogul_a_HSFILES = $(libHSmogul_a_SOURCES)
.PRECIOUS: $(libHSmogul_a_HSFILES:.hs=hi)

nobase_hi_SCRIPTS += $(libHSmogul_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSmogul_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSmogul_a_HSFILES:.hs=.hi)

DISTCLEANFILES+= mogul/libHSmogul_a.deps
-include mogul/libHSmogul_a.deps

#
#	glade package
#
################################################################################

if ENABLE_LIBGLADE

glade_PKGNAME = libHSglade_a

libHSglade_a_PACKAGE		= glade/glade.pkg
libHSglade_a_PACKAGEDEPS	= 
libHSglade_a_HEADER 		= glade/glade.h
libHSglade_a_PRECOMP		= glade/glade.precomp
libHSglade_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(LIBGLADE_LIBS)
libHSglade_a_HCFLAGS		= -fffi
libHSglade_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))
libHSglade_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))

libHSglade_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) glade
glade/libHSglade_a.deps	: gtk/libHSgtk_a.deps

libHSglade_a_GENERATEDSOURCES =	\
	glade/Graphics/UI/Gtk/Glade/Types.chs

nodist_libHSglade_a_SOURCES = $(libHSglade_a_GENERATEDSOURCES)

libHSglade_a_SOURCES =	        \
        glade/Graphics/UI/Gtk/Glade.chs

html_HSFILES_HIDDEN += $(libHSglade_a_GENERATEDSOURCES:.chs=.hs)

glade_Graphics_UI_Gtk_Glade_Types_hs_HCFLAGS	= -fglasgow-exts

glade/Graphics/UI/Gtk/Glade/Types.chs : $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
		      $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
		      $(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
		      $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
		      $@ --tag=libglade --lib=glade --prefix=glade \
                      --modname=Graphics.UI.Gtk.Glade.Types \
		      --parentname=Graphics.UI.Gtk.Types)

libHSglade_a_ALLSOURCES = $(libHSglade_a_SOURCES) $(nodist_libHSglade_a_SOURCES)

am_libHSglade_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSglade_a_ALLSOURCES))))

libHSglade_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglade_a_ALLSOURCES))
libHSglade_a_CHSFILES = \
  $(filter %.chs,$(libHSglade_a_ALLSOURCES:.chs.pp=.chs))
libHSglade_a_CHSFILES_HS = $(libHSglade_a_CHSFILES:.chs=.hs)
libHSglade_a_HSCFILES = $(filter %.hsc, $(libHSglade_a_ALLSOURCES))
libHSglade_a_HSCFILES_HS = $(libHSglade_a_HSCFILES:.hsc=.hs)
libHSglade_a_BUILDSOURCES = \
	$(libHSglade_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSglade_a_CHSFILES_HS) \
        $(libHSglade_a_HSCFILES_HS) \
        $(libHSglade_a_GENERATEDSOURCES)
libHSglade_a_HSFILES  = \
	$(filter %.hs,$(libHSglade_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSglade_a_ALLSOURCES))

nobase_hi_SCRIPTS += $(libHSglade_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSglade_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSglade_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSglade_a_BUILDSOURCES) 

DISTCLEANFILES+= glade/libHSglade_a.deps $(libHSglade_a_CHSFILES_HS:.hs=.dep)

$(libHSglade_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSglade_a_GENERATEDSOURCES)
-include $(libHSglade_a_CHSFILES:.chs=.dep) glade/libHSglade_a.deps

endif #ENABLE_LIBGLADE

#
#	gconf package
#
################################################################################

if ENABLE_GNOME

gconf_PKGNAME = libHSgconf_a

libHSgconf_a_PACKAGE		= gconf/gconf.pkg
libHSgconf_a_PACKAGEDEPS	= 
libHSgconf_a_HEADER 		= gconf/gconf-client.h
libHSgconf_a_PRECOMP		= gconf/gconf.precomp
libHSgconf_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(GCONF_LIBS)
libHSgconf_a_HCFLAGS		= -fglasgow-exts -fallow-overlapping-instances
libHSgconf_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GCONF_CFLAGS))
libHSgconf_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GCONF_CFLAGS))

libHSgconf_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) gconf
gconf/libHSgconf_a.deps	: gtk/libHSgtk_a.deps

libHSgconf_a_GENERATEDSOURCES =				\
	gconf/System/Gnome/GConf/Types.chs

nodist_libHSgconf_a_SOURCES = $(libHSgconf_a_GENERATEDSOURCES)

libHSgconf_a_SOURCES =					\
	gconf/System/Gnome/GConf/GConfValue.chs		\
	gconf/System/Gnome/GConf/GConfClient.chs	\
	gconf/System/Gnome/GConf.hs

html_HSFILES_HIDDEN += $(libHSgconf_a_GENERATEDSOURCES:.chs=.hs)

libHSgconf_a_LIBADD =	\
	gconf/System/Gnome/GConf/GConfClient_stub.o

gconf/System/Gnome/GConf/Types.chs :			\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gconf --lib=gconf --prefix=gconf \
	--modname=System.Gnome.GConf.Types --parentname=System.Glib.GObject)

libHSgconf_a_ALLSOURCES = $(libHSgconf_a_SOURCES) $(nodist_libHSgconf_a_SOURCES)

am_libHSgconf_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgconf_a_ALLSOURCES))))

libHSgconf_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgconf_a_ALLSOURCES))
libHSgconf_a_CHSFILES = \
  $(filter %.chs,$(libHSgconf_a_ALLSOURCES:.chs.pp=.chs))
libHSgconf_a_CHSFILES_HS = $(libHSgconf_a_CHSFILES:.chs=.hs)
libHSgconf_a_HSCFILES = $(filter %.hsc, $(libHSgconf_a_ALLSOURCES))
libHSgconf_a_HSCFILES_HS = $(libHSgconf_a_HSCFILES:.hsc=.hs)
libHSgconf_a_BUILDSOURCES = \
	$(libHSgconf_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgconf_a_CHSFILES_HS) \
        $(libHSgconf_a_HSCFILES_HS) \
        $(libHSgconf_a_GENERATEDSOURCES)
libHSgconf_a_HSFILES  = \
	$(filter %.hs,$(libHSgconf_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgconf_a_ALLSOURCES))

nobase_hi_SCRIPTS += $(libHSgconf_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSgconf_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSgconf_a_CHSFILES:.chs=.chi)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.c)
CLEANFILES += $(libHSgconf_a_BUILDSOURCES) 

DISTCLEANFILES+= gconf/libHSgconf_a.deps $(libHSgconf_a_CHSFILES_HS:.hs=.dep)

$(libHSgconf_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSgconf_a_GENERATEDSOURCES)
-include $(libHSgconf_a_CHSFILES:.chs=.dep) gconf/libHSgconf_a.deps

endif #ENABLE_GNOME

#
#	sourceview package
#
################################################################################

if ENABLE_GNOME

sourceview_PKGNAME = libHSsourceview_a

libHSsourceview_a_PACKAGE	= sourceview/sourceview.pkg
libHSsourceview_a_PACKAGEDEPS	= 
libHSsourceview_a_HEADER 	= sourceview.h
libHSsourceview_a_PRECOMP	= sourceview/sourceview.precomp
libHSsourceview_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(SOURCEVIEW_LIBS)
libHSsourceview_a_HCFLAGS	= -fffi
libHSsourceview_a_CFLAGS	= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS))
libHSsourceview_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS)) -Isourceview

libHSsourceview_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) sourceview
sourceview/libHSsourceview_a.deps	: gtk/libHSgtk_a.deps

libHSsourceview_a_GENERATEDSOURCES =						\
	sourceview/Graphics/UI/Gtk/SourceView/Types.chs

nodist_libHSsourceview_a_SOURCES = $(libHSsourceview_a_GENERATEDSOURCES)

libHSsourceview_a_SOURCES =							\
	sourceview/Graphics/UI/Gtk/SourceView.hs				\
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

html_HSFILES_HIDDEN += $(libHSsourceview_a_GENERATEDSOURCES:.chs=.hs)

sourceview_Graphics_UI_Gtk_SourceView_Types_hs_HCFLAGS	= -fglasgow-exts
sourceview_Graphics_UI_Gtk_SourceView_SourceTagStyle_hs_HCFLAGS	= -fglasgow-exts

sourceview/Graphics/UI/Gtk/SourceView/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=sourceview --parentname=Graphics.UI.Gtk.Types \
        --modname=Graphics.UI.Gtk.SourceView.Types)

libHSsourceview_a_ALLSOURCES = $(libHSsourceview_a_SOURCES) $(nodist_libHSsourceview_a_SOURCES)

am_libHSsourceview_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsourceview_a_ALLSOURCES))))

libHSsourceview_a_CHSPPFILES = $(filter %.chs.pp,$(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_CHSFILES = \
  $(filter %.chs,$(libHSsourceview_a_ALLSOURCES:.chs.pp=.chs))
libHSsourceview_a_CHSFILES_HS = $(libHSsourceview_a_CHSFILES:.chs=.hs)
libHSsourceview_a_HSCFILES = $(filter %.hsc, $(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_HSCFILES_HS = $(libHSsourceview_a_HSCFILES:.hsc=.hs)
libHSsourceview_a_BUILDSOURCES = \
	$(libHSsourceview_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSsourceview_a_CHSFILES_HS) \
        $(libHSsourceview_a_HSCFILES_HS) \
        $(libHSsourceview_a_GENERATEDSOURCES)
libHSsourceview_a_HSFILES  = \
	$(filter %.hs,$(libHSsourceview_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsourceview_a_ALLSOURCES))

nobase_hi_SCRIPTS += $(libHSsourceview_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSsourceview_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSsourceview_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSsourceview_a_BUILDSOURCES) 

DISTCLEANFILES+= sourceview/libHSsourceview_a.deps $(libHSsourceview_a_CHSFILES_HS:.hs=.dep)

$(libHSsourceview_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)		\
	$(libHSsourceview_a_GENERATEDSOURCES)
-include $(libHSsourceview_a_CHSFILES:.chs=.dep) sourceview/libHSsourceview_a.deps

endif #ENABLE_GNOME

#
#	mozembed package
#
################################################################################

if ENABLE_MOZEMBED

mozembed_PKGNAME = libHSmozembed_a

libHSmozembed_a_PACKAGE		= mozembed/mozembed.pkg
libHSmozembed_a_PACKAGEDEPS	= 
libHSmozembed_a_HEADER 		= gtkmozembed.h
libHSmozembed_a_PRECOMP		= mozembed/mozembed.precomp
libHSmozembed_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(MOZEMBED_LIBS)
libHSmozembed_a_HCFLAGS		= -fffi
libHSmozembed_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))
libHSmozembed_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))

libHSmozembed_a_SOURCESDIRS = $(libHSgtk_a_SOURCESDIRS) mozembed
mozembed/libHSmozembed_a.deps	: gtk/libHSgtk_a.deps

libHSmozembed_a_GENERATEDSOURCES =			\
	mozembed/Graphics/UI/Gtk/MozEmbed/Types.chs

nodist_libHSmozembed_a_SOURCES = $(libHSmozembed_a_GENERATEDSOURCES)

libHSmozembed_a_SOURCES =				\
	mozembed/Graphics/UI/Gtk/MozEmbed.chs

html_HSFILES_HIDDEN += $(libHSmozembed_a_GENERATEDSOURCES:.chs=.hs)

mozembed_Graphics_UI_Gtk_MozEmbed_Types_hs_HCFLAGS = -fglasgow-exts

mozembed/Graphics/UI/Gtk/MozEmbed/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=mozembed --parentname=Graphics.UI.Gtk.Types \
	--modname=Graphics.UI.Gtk.MozEmbed.Types)

libHSmozembed_a_ALLSOURCES = $(libHSmozembed_a_SOURCES) $(nodist_libHSmozembed_a_SOURCES)

am_libHSmozembed_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSmozembed_a_ALLSOURCES))))

libHSmozembed_a_CHSPPFILES = $(filter %.chs.pp,$(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_CHSFILES = $(filter %.chs,$(libHSmozembed_a_ALLSOURCES:.chs.pp=.chs))
libHSmozembed_a_CHSFILES_HS = $(libHSmozembed_a_CHSFILES:.chs=.hs)
libHSmozembed_a_HSCFILES = $(filter %.hsc, $(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_HSCFILES_HS = $(libHSmozembed_a_HSCFILES:.hsc=.hs)
libHSmozembed_a_BUILDSOURCES = \
	$(libHSmozembed_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSmozembed_a_CHSFILES_HS) \
        $(libHSmozembed_a_HSCFILES_HS) \
        $(libHSmozembed_a_GENERATEDSOURCES)
libHSmozembed_a_HSFILES  = \
	$(filter %.hs,$(libHSmozembed_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSmozembed_a_ALLSOURCES))

nobase_hi_SCRIPTS += $(libHSmozembed_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSmozembed_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSmozembed_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSmozembed_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSmozembed_a_BUILDSOURCES) 

DISTCLEANFILES+= mozembed/libHSmozembed_a.deps $(libHSmozembed_a_CHSFILES_HS:.hs=.dep)

$(libHSmozembed_a_CHSFILES:.chs=.dep) : \
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSmozembed_a_GENERATEDSOURCES)
-include $(libHSmozembed_a_CHSFILES:.chs=.dep) mozembed/libHSmozembed_a.deps

endif #ENABLE_MOZEMBED

#
#	documentation
#
################################################################################

if BUILDDOCS

# For now just build all the docs together, we can think about doing it in a
# more modular fashion later if it turns out to be useful.

htmldir = $(datadir)/doc/gtk2hs/html

PACKAGES = $(foreach PACKAGE,$(pkglib_LIBRARIES), $(patsubst libHS%.a,%,$(PACKAGE)))

html_HSFILES =  $(filter-out $(html_HSFILES_EXCLUDE) $(html_HSFILES_PREPROC),	\
		$(foreach PACKAGE,$(PACKAGES),$(libHS$(PACKAGE)_a_HSFILES)))	\
		$(html_HSFILES_PREPROC:.hs=.hs.uncpp)

html_DATA =									\
	$(foreach HSFILE, $(filter-out $(html_HSFILES_HIDDEN),$(html_HSFILES)),	\
	    $(patsubst $(firstword $(subst /, ,$(HSFILE))).%.hs, doc/%.html, 	\
		$(subst /,.,$(HSFILE:.uncpp=))))				\
	doc/haddock.css doc/haskell_icon.gif					\
	doc/index.html doc/doc-index.html					\
	$(foreach LETTER, A B C D E F G H I J K L M N O P R S T U V W X Y,	\
		doc/doc-index-$(LETTER).html) #missing letters Q and Z

haddockifacedir = $(datadir)/doc/gtk2hs
haddockiface_DATA = doc/gtk2hs.haddock

# --prologue=doc/prologue.txt

$(html_DATA) doc/gtk2hs.haddock : $(html_HSFILES)
	mkdir -p doc
	$(strip $(HADDOCK) --html --odir=doc \
		--title="Gtk2Hs" \
		--dump-interface=doc/gtk2hs.haddock \
		$^)

.hs.hs.uncpp : $(CONFIG_H)
	$(HSCPP) -I. $< -o $@

CLEANFILES += $(html_DATA) $(html_HSFILES_PREPROC:.hs=.hs.uncpp)

endif #BUILDDOCS

#
#	demos
#
################################################################################

DEMOS = demo/hello demo/buttonbox demo/concurrent \
	demo/graphic demo/treeList demo/unicode
if HAVE_GTK_VERSION_2_4
DEMOS += demo/filechooser 
endif
if ENABLE_LIBGLADE
DEMOS += demo/glade
if HAVE_GTK_VERSION_2_4
DEMOS += demo/profileviewer
endif
endif
if ENABLE_GNOME
DEMOS += demo/gconf demo/sourceview
endif
if ENABLE_MOZEMBED
DEMOS += demo/mozembed
endif

# For the install check just make sure all the demo programs compile and link
DEMO_MAKE_ARGS= $(if $(PKGCONF), HCFLAGS="-package-conf $(DESTDIR)$(PKGCONF)") \
		$(if $(GHCPKG_USE_AUTOLIBS),, HCNEEDSPACKAGE=yes)

installcheck-local:
	$(foreach DEMO,$(DEMOS),$(strip	\
		make -C $(DEMO) clean &&\
		make -C $(DEMO) $(DEMO_MAKE_ARGS) &&	\
	)) echo All demos compiled ok, now try running one or two...

EXTRA_DIST +=								\
	demo/buttonbox/ButtonBox.hs demo/buttonbox/Makefile		\
	demo/concurrent/Progress.hs demo/concurrent/Makefile 		\
	demo/filechooser/FileChooserDemo.hs demo/filechooser/Makefile	\
		demo/filechooser/FileChooserDemo.glade			\
	demo/gconf/GConfDemo.hs demo/gconf/Makefile			\
	demo/glade/GladeTest.hs demo/glade/Makefile			\
		demo/glade/simple.glade 				\
	demo/graphic/Drawing.hs demo/graphic/Makefile			\
	demo/hello/World.hs demo/hello/Makefile 			\
	demo/mozembed/TestEmbedMoz.hs demo/mozembed/Makefile		\
	demo/profileviewer/ProfileViewer.hs				\
		demo/profileviewer/ParseProfile.hs			\
		demo/profileviewer/ProfileViewer.glade			\
		demo/profileviewer/Makefile				\
	demo/sourceview/SourceViewTest.hs demo/sourceview/Makefile	\
		demo/sourceview/haskell.lang				\
	demo/treeList/ListTest.hs demo/treeList/Makefile		\
	demo/unicode/Arabic.hs demo/unicode/Makefile

#
#       installing and registering packages with the Haskell compiler
#
################################################################################

# Include all the .pkg files in the install set, they're useful to distributors
# and if your installation gets corrupt you can re-register the packages from them.
pkglib_DATA = $(foreach pkgname,$(pkglib_LIBRARIES), $(call getVar,$(pkgname),PACKAGE))

if ENABLE_PKGREG

# For distributors we usually do not want to register the packages using
# ghc-pkg at install time. That is usually left for a post-install action.
# Therefore these actions can be disabled by ./configure --without-pkgreg

install-data-hook :
	$(if $(PKGCONF),if test -f $(PKGCONF); then :; \
	else echo "[]" > $(PKGCONF); fi;)
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	rm -f $(DESTDIR)$(pkglibdir)/$(patsubst lib%.a,%.o,$(pkgname)); \
	prefix=$(prefix) exec_prefix=$(exec_prefix) pkglibdir=$(pkglibdir) \
	$(GHCPKG) $(addprefix --config-file=,$(PKGCONF)) \
	--update-package $(GHCPKG_BUILD_GHCI_LIB) \
	--input-file=$(call getVar,$(pkgname),PACKAGE);)

uninstall-hook :
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	  $(GHCPKG) $(addprefix --config-file=,$(PKGCONF)) \
	  --remove-package=`cat $(call getVar,$(pkgname),PACKAGE) \
	  | $(GREP) name | $(SED) "s/ *name *= *\"\([a-zA-Z0-9]*\)\",/\1/"`;) \
	$(if $(PKGCONF),if test -f $(PKGCONF); then \
	  if test -n `head $(PKGCONF) | $(GREP) -e "\[\]"`; then \
	  $(RM) $(PKGCONF) $(PKGCONF).old; fi; \
	fi)
else

install-data-hook :
uninstall-hook :

endif

include mk/common.mk
