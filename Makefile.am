AUTOMAKE_OPTIONS = 1.8 foreign no-dependencies
SUFFIXES = .hs.uncpp .chs.pp .chs .hsc .deps .dep
MOSTLYCLEANFILES = */*.deps.bak tools/*/*.deps.bak
CLEANFILES = */*.precomp
DISTCLEANFILES =

EXTRA_DIST = \
	tools/callbackGen/Signal.chs.template	\
	tools/callbackGen/gtkmarshal.list	\
	tools/checkDirs.sh			\
	tools/c2hs/toplevel/c2hs_config.h	\
	tools/hierarchyGen/hierarchy.list	\
	tools/hierarchyGen/Hierarchy.chs.template \
        gconf/System/Gnome/GConf/marshal.list	\
	sourceview/sourceview.h			\
	gtk/wingtk.h				\
        glib/System/Glib/hsgclosure.h

# There are no other files in these dirs and tar excludes empty dirs
dist-hook:
	mkdir $(distdir)/glade/Graphics/UI/Gtk/Glade
	touch $(distdir)/glade/Graphics/UI/Gtk/Glade/.keep
	mkdir $(distdir)/mozembed/Graphics/UI/Gtk/MozEmbed
	touch $(distdir)/mozembed/Graphics/UI/Gtk/MozEmbed/.keep

CHSDEPEND = $(srcdir)/mk/chsDepend

# all packages and applications
pkglib_LIBRARIES =	\
        libHSglib.a
if ENABLE_CAIRO
pkglib_LIBRARIES += libHScairo.a
endif
pkglib_LIBRARIES +=	\
	libHSgtk.a	\
        libHSmogul.a
if ENABLE_LIBGLADE
pkglib_LIBRARIES += libHSglade.a
endif
if ENABLE_GCONF
pkglib_LIBRARIES += libHSgconf.a
endif
if ENABLE_SOURCEVIEW
pkglib_LIBRARIES += libHSsourceview.a
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
tools_callbackGen_HookGenerator_hs_HCFLAGS = -I. 
am_tools_callbackGen_HookGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_callbackGen_HookGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_callbackGen_HookGenerator_OBJECTS)
MOSTLYCLEANFILES+= $(tools_callbackGen_HookGenerator_SOURCES:.hs=.hi)

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

tools/c2hs/c/CLexer.hs : tools/c2hs/c/CLexer.x
	$(ALEX) --ghc -o $@ $<

tools/c2hs/c/CParser.hs : tools/c2hs/c/CParser.y
	$(HAPPY) --ghc --array --coerce --strict -o $@ $<

MAINTAINERCLEANFILES =				\
	tools/c2hs/c/CParser.hs			\
	tools/c2hs/c/CLexer.hs

EXTRA_DIST +=					\
	tools/c2hs/c/CParser.y			\
	tools/c2hs/c/CLexer.x

tools_c2hs_c2hsLocal_HSFILES		= \
	$(filter %.hs,$(tools_c2hs_c2hsLocal_ALLSOURCES))
# Fix automake - the subdir-objects option doesn't work here.
am_tools_c2hs_c2hsLocal_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(tools_c2hs_c2hsLocal_ALLSOURCES)))
MOSTLYCLEANFILES+= $(am_tools_c2hs_c2hsLocal_OBJECTS)
MOSTLYCLEANFILES+= $(tools_c2hs_c2hsLocal_HSFILES:.hs=.hi)
CLEANFILES+= tools/c2hs/c2hsLocal.deps

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include tools/c2hs/c2hsLocal.deps
 endif

#
#	glib package
#
################################################################################

glib_PKGNAME = libHSglib_a

libHSglib_a_PACKAGE		= glib/glib.$(PKGEXT)
libHSglib_a_PACKAGEDEPS		= 
libHSglib_a_HEADER 		= glib-object.h
libHSglib_a_PRECOMP		= glib/glib.precomp
libHSglib_a_LIBS		= $(GLIB_LIBS)
libHSglib_a_HCFLAGS		= -fffi $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
libHSglib_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS))
libHSglib_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS))

libHSglib_a_SOURCESDIRS = glib

libHSglib_a_SOURCES =	        		\
	glib/System/Glib.hs			\
	glib/System/Glib/FFI.hs			\
	glib/System/Glib/UTFString.hs		\
	glib/System/Glib/Types.chs		\
	glib/System/Glib/GType.chs		\
	glib/System/Glib/GTypeConstants.hsc	\
	glib/System/Glib/GValue.chs		\
	glib/System/Glib/GValueTypes.chs	\
	glib/System/Glib/GParameter.hsc		\
	glib/System/Glib/StoreValue.hsc		\
	glib/System/Glib/GObject.chs.pp		\
	glib/System/Glib/Properties.chs		\
	glib/System/Glib/Attributes.hs		\
	glib/System/Glib/GError.chs.pp		\
	glib/System/Glib/GList.chs		\
	glib/System/Glib/Signals.chs.pp		\
        glib/System/Glib/Flags.hs

if USE_GCLOSUE_SIGNALS_IMPL
libHSglib_a_SOURCES += glib/System/Glib/hsgclosure.c
# make calling an undefined function an error so we can catch changes in the
# GHC RTS API:
glib_System_Glib_hsgclosure_c_CFLAGS = -Werror-implicit-function-declaration
endif

html_HSFILES_EXCLUDE =				\
	glib/System/Glib/Signals.hs

html_HSFILES_PREPROC = glib/System/Glib/FFI.hs
html_HSFILES_HIDDEN  = glib/System/Glib/FFI.hs \
		       gtk/Graphics/UI/Gtk/Pango/Types.hs

glib_System_Glib_Types_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_GError_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_Attributes_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_Signals_hs_HCFLAGS	= '-\#include"hsgclosure.h"'

libHSglib_a_LIBADD  = glib/System/Glib/GObject_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSglib_a_LIBADD += glib/System/Glib/Signals_stub.o
endif

am_libHSglib_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSglib_a_SOURCES))))

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

nobase_hi_DATA = $(libHSglib_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSglib_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSglib_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSglib_a_CHSFILES:.chs=.chi)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.h)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.o)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.c)
CLEANFILES += $(libHSglib_a_BUILDSOURCES)

CLEANFILES+= glib/libHSglib_a.deps $(libHSglib_a_CHSFILES_HS:.hs=.dep)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSglib_a_CHSFILES:.chs=.dep) glib/libHSglib_a.deps
 endif

#
#	gtk package
#
################################################################################

gtk_PKGNAME = libHSgtk_a

libHSgtk_a_PACKAGE		= gtk/gtk.$(PKGEXT)
libHSgtk_a_PACKAGEDEPS		= 
libHSgtk_a_HEADER 		= gtk/gtk.h
libHSgtk_a_PRECOMP		= gtk/gtk.precomp
libHSgtk_a_LIBS			= $(GLIB_LIBS) $(GTK_LIBS)
libHSgtk_a_HCFLAGS		= -fffi $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
libHSgtk_a_CFLAGS 		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))
libHSgtk_a_CPPFLAGS 		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))

libHSgtk_a_SOURCESDIRS = $(libHSglib_a_SOURCESDIRS) gtk
gtk/libHSgtk_a.deps	: glib/libHSglib_a.deps

if ENABLE_CAIRO
libHSgtk_a_SOURCESDIRS += cairo
gtk/libHSgtk_a.deps	: cairo/libHScairo_a.deps
endif

libHSgtk_a_GENERATEDSOURCES =						\
	gtk/Graphics/UI/Gtk/Types.chs					\
	gtk/Graphics/UI/Gtk/Signals.chs

nodist_libHSgtk_a_SOURCES = $(libHSgtk_a_GENERATEDSOURCES)

libHSgtk_a_SOURCES =					 		\
	gtk/Graphics/UI/Gtk.hs						\
	gtk/Graphics/UI/Gtk/Pango/Types.chs.pp				\
	gtk/Graphics/UI/Gtk/Multiline/TextIter.chs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Region.chs.pp				\
	gtk/Graphics/UI/Gtk/Abstract/Bin.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Box.chs				\
	gtk/Graphics/UI/Gtk/Abstract/ButtonBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Container.chs			\
	gtk/Graphics/UI/Gtk/Abstract/ContainerChildProperties.chs	\
	gtk/Graphics/UI/Gtk/Abstract/Misc.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Object.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Paned.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Range.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Scale.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Widget.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/Button.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/CheckButton.chs			\
	gtk/Graphics/UI/Gtk/Buttons/RadioButton.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/ToggleButton.chs			\
	gtk/Graphics/UI/Gtk/Display/AccelLabel.chs			\
	gtk/Graphics/UI/Gtk/Display/Image.chs.pp			\
	gtk/Graphics/UI/Gtk/Display/Label.chs.pp			\
	gtk/Graphics/UI/Gtk/Display/ProgressBar.chs.pp			\
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
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuBar.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuShell.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/TearoffMenuItem.chs	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Toolbar.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToolButton.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToggleToolButton.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/SeparatorMenuItem.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/SeparatorToolItem.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/Action.chs.pp		\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/ActionGroup.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs.pp		\
	gtk/Graphics/UI/Gtk/Misc/Adjustment.chs.pp				\
	gtk/Graphics/UI/Gtk/Misc/Calendar.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/DrawingArea.chs			\
	gtk/Graphics/UI/Gtk/Misc/EventBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Arrow.chs				\
	gtk/Graphics/UI/Gtk/Misc/HandleBox.chs				\
	gtk/Graphics/UI/Gtk/Misc/SizeGroup.chs.pp				\
	gtk/Graphics/UI/Gtk/Misc/Tooltips.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Viewport.chs				\
	gtk/Graphics/UI/Gtk/Multiline/TextBuffer.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextMark.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextTag.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextTagTable.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextView.chs.pp			\
	gtk/Graphics/UI/Gtk/Ornaments/Frame.chs				\
	gtk/Graphics/UI/Gtk/Ornaments/HSeparator.chs			\
	gtk/Graphics/UI/Gtk/Ornaments/VSeparator.chs			\
	gtk/Graphics/UI/Gtk/Scrolling/HScrollbar.chs			\
	gtk/Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs.pp		\
	gtk/Graphics/UI/Gtk/Scrolling/VScrollbar.chs			\
	gtk/Graphics/UI/Gtk/Selectors/ColorButton.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/ColorSelection.chs		\
	gtk/Graphics/UI/Gtk/Selectors/ColorSelectionDialog.chs		\
	gtk/Graphics/UI/Gtk/Selectors/FontButton.chs.pp			\
	gtk/Graphics/UI/Gtk/Selectors/FontSelection.chs			\
	gtk/Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs		\
	gtk/Graphics/UI/Gtk/Selectors/FileSelection.chs.pp              \
	gtk/Graphics/UI/Gtk/Selectors/FileChooser.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserButton.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserWidget.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserDialog.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileFilter.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererPixbuf.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererText.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererToggle.chs		\
	gtk/Graphics/UI/Gtk/TreeList/ListStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreePath.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeIter.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeRowReference.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModelSort.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeViewColumn.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/IconView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/CellView.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/AboutDialog.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/Dialog.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/Window.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/WindowGroup.chs			\
	gtk/Graphics/UI/Gtk/Gdk/Drawable.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/GC.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Gdk.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Enums.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Keys.chs				\
	gtk/Graphics/UI/Gtk/Gdk/PixbufData.hs				\
	gtk/Graphics/UI/Gtk/Gdk/Pixbuf.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/Pixmap.chs				\
	gtk/Graphics/UI/Gtk/Pango/Description.chs			\
	gtk/Graphics/UI/Gtk/Pango/Enums.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/GlyphStorage.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Layout.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Rendering.chs.pp			\
	gtk/Graphics/UI/Gtk/General/StockItems.hsc			\
	gtk/Graphics/UI/Gtk/General/Structs.hsc				\
	gtk/Graphics/UI/Gtk/Gdk/Events.hsc				\
	gtk/Graphics/UI/Gtk/Abstract/Scrollbar.hs			\
	gtk/Graphics/UI/Gtk/Abstract/Separator.hs			\
	gtk/Graphics/UI/Gtk/TreeList/CellRenderer.hs			\
	gtk/Graphics/UI/Gtk/Gdk/DrawWindow.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Markup.hs                             \
        gtk/Graphics/UI/Gtk/Cairo.chs

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

html_HSFILES_PREPROC += gtk/Graphics/UI/Gtk.hs
html_HSFILES_HIDDEN  += $(libHSgtk_a_GENERATEDSOURCES:.chs=.hs)		\
			gtk/Graphics/UI/Gtk/Gdk/PixbufData.hs
html_HSFILES_HIDDEN += gtk/Graphics/UI/Gtk/Signals.chs

gtk_Graphics_UI_Gtk_hs_HCFLAGS			= -fno-warn-duplicate-exports	#FIXME
gtk_Graphics_UI_Gtk_Gdk_Enums_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Types_hs_HCFLAGS	        = -fglasgow-exts
gtk_Graphics_UI_Gtk_General_Structs_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Events_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_General_StockItems_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Region_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Drawable_hs_HCFLAGS	= -fglasgow-exts

if WIN32
gtk_Graphics_UI_Gtk_General_General_hs_HCFLAGS	= '-\#include"gtk/wingtk.h"' -I.
endif

libHSgtk_a_LIBADD =							\
	gtk/Graphics/UI/Gtk/Abstract/Container_stub.o			\
	gtk/Graphics/UI/Gtk/Multiline/TextIter_stub.o			\
	gtk/Graphics/UI/Gtk/Entry/EntryCompletion_stub.o		\
        gtk/Graphics/UI/Gtk/Multiline/TextTagTable_stub.o		\
	gtk/Graphics/UI/Gtk/General/General_stub.o			\
        gtk/Graphics/UI/Gtk/TreeList/TreeModel_stub.o			\
        gtk/Graphics/UI/Gtk/TreeList/TreeSelection_stub.o		\
        gtk/Graphics/UI/Gtk/TreeList/TreeView_stub.o			\
	gtk/Graphics/UI/Gtk/TreeList/IconView_stub.o			\
	gtk/Graphics/UI/Gtk/Selectors/FileFilter_stub.o			\
	gtk/Graphics/UI/Gtk/Windows/AboutDialog_stub.o			\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/ActionGroup_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgtk_a_LIBADD += gtk/Graphics/UI/Gtk/Signals_stub.o
endif

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

gtk/Graphics/UI/Gtk/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/tools/callbackGen/gtkmarshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/tools/callbackGen/gtkmarshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	Graphics.UI.Gtk.Signals)

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

nobase_hi_DATA += $(libHSgtk_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSgtk_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgtk_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSgtk_a_CHSFILES:.chs=.chi)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.c)
CLEANFILES+= $(libHSgtk_a_BUILDSOURCES)

CLEANFILES+= gtk/libHSgtk_a.deps $(libHSgtk_a_CHSFILES_HS:.hs=.dep)

$(libHSgtk_a_CHSFILES:.chs=.dep) : $(libHSgtk_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgtk_a_CHSFILES_HS:.hs=.dep) gtk/libHSgtk_a.deps
 endif

#
#	mogul package
#
################################################################################

mogul_PKGNAME = libHSmogul_a

libHSmogul_a_PACKAGE		= mogul/mogul.$(PKGEXT)
libHSmogul_a_PACKAGEDEPS	= 
libHSmogul_a_LIBS		= 
libHSmogul_a_HCFLAGS		= -I. $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
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

nobase_hi_DATA += $(libHSmogul_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSmogul_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSmogul_a_HSFILES:.hs=.hi)

CLEANFILES+= mogul/libHSmogul_a.deps

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include mogul/libHSmogul_a.deps
 endif

#
#	glade package
#
################################################################################

if ENABLE_LIBGLADE

glade_PKGNAME = libHSglade_a

libHSglade_a_PACKAGE		= glade/glade.$(PKGEXT)
libHSglade_a_PACKAGEDEPS	= 
libHSglade_a_HEADER 		= glade/glade.h
libHSglade_a_PRECOMP		= glade/glade.precomp
libHSglade_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(LIBGLADE_LIBS)
libHSglade_a_HCFLAGS		= -fffi $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
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

nobase_hi_DATA += $(libHSglade_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSglade_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSglade_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSglade_a_BUILDSOURCES) 

CLEANFILES+= glade/libHSglade_a.deps $(libHSglade_a_CHSFILES_HS:.hs=.dep)

$(libHSglade_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSglade_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSglade_a_CHSFILES:.chs=.dep) glade/libHSglade_a.deps
 endif

endif #ENABLE_LIBGLADE

#
#	gconf package
#
################################################################################

if ENABLE_GCONF

gconf_PKGNAME = libHSgconf_a

libHSgconf_a_PACKAGE		= gconf/gconf.$(PKGEXT)
libHSgconf_a_PACKAGEDEPS	= 
libHSgconf_a_HEADER 		= gconf/gconf-client.h
libHSgconf_a_PRECOMP		= gconf/gconf.precomp
libHSgconf_a_LIBS		= $(GLIB_LIBS) $(GCONF_LIBS)
libHSgconf_a_HCFLAGS		= -fglasgow-exts -fallow-overlapping-instances \
                                   $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
libHSgconf_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GCONF_CFLAGS))
libHSgconf_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GCONF_CFLAGS))

libHSgconf_a_SOURCESDIRS = $(libHSglib_a_SOURCESDIRS) gconf
gconf/libHSgconf_a.deps	: glib/libHSglib_a.deps

libHSgconf_a_GENERATEDSOURCES =				\
	gconf/System/Gnome/GConf/Types.chs		\
	gconf/System/Gnome/GConf/Signals.chs

nodist_libHSgconf_a_SOURCES = $(libHSgconf_a_GENERATEDSOURCES)

libHSgconf_a_SOURCES =					\
	gconf/System/Gnome/GConf/GConfValue.chs		\
	gconf/System/Gnome/GConf/GConfClient.chs	\
	gconf/System/Gnome/GConf.hs

html_HSFILES_HIDDEN += $(libHSgconf_a_GENERATEDSOURCES:.chs=.hs)

libHSgconf_a_LIBADD =	\
	gconf/System/Gnome/GConf/GConfClient_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgconf_a_LIBADD +=	\
	gconf/System/Gnome/GConf/Signals_stub.o
endif

gconf/System/Gnome/GConf/Types.chs :			\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gconf --lib=gconf --prefix=gconf \
	--modname=System.Gnome.GConf.Types --parentname=System.Glib.GObject)

gconf/System/Gnome/GConf/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/gconf/System/Gnome/GConf/marshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/gconf/System/Gnome/GConf/marshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	System.Gnome.GConf.Signals)

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

nobase_hi_DATA += $(libHSgconf_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSgconf_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += \
	$(libHSgconf_a_CHSFILES:.chs=.chi)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.c)
CLEANFILES += $(libHSgconf_a_BUILDSOURCES) 

CLEANFILES+= gconf/libHSgconf_a.deps $(libHSgconf_a_CHSFILES_HS:.hs=.dep)

$(libHSgconf_a_CHSFILES:.chs=.dep) :	\
	$(libHSgconf_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgconf_a_CHSFILES:.chs=.dep) gconf/libHSgconf_a.deps
 endif

endif #ENABLE_GCONF

#
#	sourceview package
#
################################################################################

if ENABLE_SOURCEVIEW

sourceview_PKGNAME = libHSsourceview_a

libHSsourceview_a_PACKAGE	= sourceview/sourceview.$(PKGEXT)
libHSsourceview_a_PACKAGEDEPS	= 
libHSsourceview_a_HEADER 	= sourceview.h
libHSsourceview_a_PRECOMP	= sourceview/sourceview.precomp
libHSsourceview_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(SOURCEVIEW_LIBS)
libHSsourceview_a_HCFLAGS	= -fffi $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
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

nobase_hi_DATA += $(libHSsourceview_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSsourceview_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSsourceview_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSsourceview_a_BUILDSOURCES) 

CLEANFILES+= sourceview/libHSsourceview_a.deps $(libHSsourceview_a_CHSFILES_HS:.hs=.dep)

$(libHSsourceview_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)		\
	$(libHSsourceview_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSsourceview_a_CHSFILES:.chs=.dep) sourceview/libHSsourceview_a.deps
 endif

endif #ENABLE_SOURCEVIEW

#
#	mozembed package
#
################################################################################

if ENABLE_MOZEMBED

mozembed_PKGNAME = libHSmozembed_a

libHSmozembed_a_PACKAGE		= mozembed/mozembed.$(PKGEXT)
libHSmozembed_a_PACKAGEDEPS	= 
libHSmozembed_a_HEADER 		= gtkmozembed.h
libHSmozembed_a_PRECOMP		= mozembed/mozembed.precomp
libHSmozembed_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(MOZEMBED_LIBS)
libHSmozembed_a_HCFLAGS		= -fffi $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
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

nobase_hi_DATA += $(libHSmozembed_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSmozembed_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSmozembed_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSmozembed_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSmozembed_a_BUILDSOURCES) 

CLEANFILES+= mozembed/libHSmozembed_a.deps $(libHSmozembed_a_CHSFILES_HS:.hs=.dep)

$(libHSmozembed_a_CHSFILES:.chs=.dep) : \
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSmozembed_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSmozembed_a_CHSFILES:.chs=.dep) mozembed/libHSmozembed_a.deps
 endif

endif #ENABLE_MOZEMBED

#
#	cairo package
#
################################################################################

if ENABLE_CAIRO

cairo_PKGNAME = libHScairo_a

libHScairo_a_PACKAGE		= cairo/cairo.$(PKGEXT)
libHScairo_a_PACKAGEDEPS	= 
libHScairo_a_HEADER 		= cairo/cairo.h
libHScairo_a_PRECOMP		= cairo/cairo.precomp
libHScairo_a_LIBS		= $(CAIRO_LIBS)
libHScairo_a_HCFLAGS		= -fffi $(if $(USE_NEW_PKG_FORMAT),-ignore-package glib)
libHScairo_a_CFLAGS		= $(filter-out -I% -D%,$(CAIRO_CFLAGS))
libHScairo_a_CPPFLAGS		= $(filter -I% -D%,$(CAIRO_CFLAGS)) -I.

libHScairo_a_SOURCESDIRS = $(libHSglib_a_SOURCESDIRS) cairo
cairo/libHScairo_a.deps: glib/libHSglib_a.deps

libHScairo_a_GENERATEDSOURCES =

nodist_libHScairo_a_SOURCES = $(libHScairo_a_GENERATEDSOURCES)

libHScairo_a_SOURCES =							\
	cairo/Graphics/Rendering/Cairo.hs                               \
	cairo/Graphics/Rendering/Cairo/Matrix.chs                       \
	cairo/Graphics/Rendering/Cairo/Types.chs                        \
	cairo/Graphics/Rendering/Cairo/Internal.hs			\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Cairo.chs       \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Paths.chs       \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Patterns.chs    \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Text.chs        \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Transformations.chs \
	cairo/Graphics/Rendering/Cairo/Internal/Fonts/FontOptions.chs   \
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Image.chs      \
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PNG.chs        \
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Surface.chs    \
	cairo/Graphics/Rendering/Cairo/Internal/Utilities.chs
#	cairo/Graphics/Rendering/Cairo/PDF.hs
#	cairo/Graphics/Rendering/Cairo/PS.hs
#	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PDF.chs
#	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PS.chs

cairo_Graphics_Rendering_Cairo_hs_HCFLAGS = -fglasgow-exts
cairo_Graphics_Rendering_Cairo_Types_hs_HCFLAGS = -fglasgow-exts

html_HSFILES_HIDDEN += $(libHScairo_a_GENERATEDSOURCES:.chs=.hs)
html_HSFILES_EXCLUDE +=							\
	cairo/Graphics/Rendering/Cairo/Types.hs				\
	cairo/Graphics/Rendering/Cairo/Internal.hs			\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Cairo.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Paths.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Patterns.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Text.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Transformations.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Fonts/FontOptions.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Image.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PNG.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Surface.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Utilities.hs

libHScairo_a_ALLSOURCES = $(libHScairo_a_SOURCES) $(nodist_libHScairo_a_SOURCES)

am_libHScairo_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHScairo_a_ALLSOURCES))))

libHScairo_a_CHSPPFILES = $(filter %.chs.pp,$(libHScairo_a_ALLSOURCES))
libHScairo_a_CHSFILES = \
  $(filter %.chs,$(libHScairo_a_ALLSOURCES:.chs.pp=.chs))
libHScairo_a_CHSFILES_HS = $(libHScairo_a_CHSFILES:.chs=.hs)
libHScairo_a_HSCFILES = $(filter %.hsc, $(libHScairo_a_ALLSOURCES))
libHScairo_a_HSCFILES_HS = $(libHScairo_a_HSCFILES:.hsc=.hs)
libHScairo_a_BUILDSOURCES = \
	$(libHScairo_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHScairo_a_CHSFILES_HS) \
        $(libHScairo_a_HSCFILES_HS) \
        $(libHScairo_a_GENERATEDSOURCES)
libHScairo_a_HSFILES  = \
	$(filter %.hs,$(libHScairo_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHScairo_a_ALLSOURCES))

nobase_hi_DATA += $(libHScairo_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHScairo_a_OBJECTS)
MOSTLYCLEANFILES += $(libHScairo_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHScairo_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHScairo_a_BUILDSOURCES) 

CLEANFILES+= cairo/libHScairo_a.deps $(libHScairo_a_CHSFILES_HS:.hs=.dep)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHScairo_a_CHSFILES:.chs=.dep) cairo/libHScairo_a.deps
 endif

endif #ENABLE_CAIRO

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

if HYPHENATED_DOCS
DOCS_SEP =-
else
DOCS_SEP =.
endif # HYPHENATED_DOCS

html_DATA =									  \
	$(foreach HSFILE, $(filter-out						  \
	$(html_HSFILES_HIDDEN),$(html_HSFILES)),				  \
	    $(patsubst $(firstword $(subst /,					  \
	    ,$(HSFILE)))$(DOCS_SEP)%.hs,					  \
	     docs/reference/%.html,						  \
		$(subst /,$(DOCS_SEP),$(HSFILE:.uncpp=))))			  \
	docs/reference/haddock.css docs/reference/haskell_icon.gif		  \
	docs/reference/index.html docs/reference/doc-index.html			  \
	docs/reference/gtk2hs.haddock						  \
	$(foreach LETTER, A B C D E F G H I J K L M N O P R S T U V W X Y 58,	  \
		docs/reference/doc-index-$(LETTER).html) #missing letters Q and Z

$(html_DATA) : $(html_HSFILES)
	mkdir -p docs/reference
	$(strip $(HADDOCK) --html --odir=docs/reference \
		--title="Gtk2Hs" \
		--dump-interface=docs/reference/gtk2hs.haddock \
		--prologue=docs/prologue.txt \
		$^)

.hs.hs.uncpp : $(CONFIG_HEADER)
	$(HSCPP) -I. $< -o $@

CLEANFILES += $(html_DATA) $(html_HSFILES_PREPROC:.hs=.hs.uncpp)

endif #BUILDDOCS

#
#	demos
#
################################################################################

DEMOS = demo/hello demo/buttonbox demo/concurrent \
	demo/graphic demo/unicode
if HAVE_GTK_VERSION_2_2
DEMOS += demo/treeList demo/fastdraw
endif
if ENABLE_LIBGLADE
DEMOS += demo/glade demo/calc
if HAVE_GTK_VERSION_2_4
DEMOS += demo/filechooser demo/profileviewer
endif
endif
if ENABLE_GCONF
DEMOS += demo/gconf
endif
if ENABLE_SOURCEVIEW
DEMOS += demo/sourceview
endif
if ENABLE_MOZEMBED
DEMOS += demo/mozembed
endif

# For the install check just make sure all the demo programs compile and link
DEMO_MAKE_ARGS= $(if $(PKGCONF), HCFLAGS="-package-conf $(PKGCONF)") \
		$(if $(GHCPKG_USE_AUTOLIBS),, HCNEEDSPACKAGE=yes) HC=$(HC)

installcheck-local:
	$(foreach DEMO,$(DEMOS),$(strip	\
		$(MAKE) -C $(DEMO) clean &&\
		$(MAKE) -C $(DEMO) $(DEMO_MAKE_ARGS) &&	\
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
	demo/unicode/Arabic.hs demo/unicode/Makefile			\
	demo/calc/CalcModel.hs demo/calc/Calc.hs demo/calc/Makefile	\
		demo/calc/calc.glade					\
	demo/fastdraw/FastDraw.hs demo/fastdraw/Makefile

#
#       installing and registering packages with the Haskell compiler
#
################################################################################

# Include all the .pkg/.package.conf files in the install set, they're useful
# to distributors and if your installation gets corrupt you can re-register the
# packages from them.
pkglib_DATA = $(foreach pkgname,$(pkglib_LIBRARIES), $(call getVar,$(pkgname),PACKAGE))

# Before we install the .pkg/.package.conf files we need to substitute for the
# ${pkglibdir} and ${modules} variables.
if !WIN32_INSTALLER
install-exec-local:
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$${pkglibdir}|$(pkglibdir)|g' \
		-e 's|$${modules}|$(call getModules,$(pkgname))|' \
		$(call getVar,$(pkgname),PACKAGE) \
	> $(call getVar,$(pkgname),PACKAGE).tmp; \
	mv $(call getVar,$(pkgname),PACKAGE).tmp $(call getVar,$(pkgname),PACKAGE); \
	$(SED)	's|$${modules}|$(call getModules,$(pkgname))|' \
		$(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),PACKAGE)) \
	> $(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),PACKAGE)).tmp; \
	mv $(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),PACKAGE)).tmp \
	   $(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),PACKAGE));)

else WIN32_INSTALLER
# Except on Windows if we're makeing an installer where we delay the variable
# substitution, since the installer does that. So we still substitute for the
# ${modules} variable, but we don't substitute for ${pkglibdir} and we
# "reverse" substitute occurences of the Gtk installation prefix for
# ${GTK_BASEPATH} (that is the string literal, not the value of the variable)
install-exec-local:
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$(GTK_BASEPATH)|$${GTK_BASEPATH}|g' \
		-e 's|$${modules}|$(call getModules,$(pkgname))|' \
		$(call getVar,$(pkgname),PACKAGE) \
	> $(call getVar,$(pkgname),PACKAGE).tmp; \
	mv $(call getVar,$(pkgname),PACKAGE).tmp $(call getVar,$(pkgname),PACKAGE);)
endif WIN32_INSTALLER

COMMA := ,
getModules =									\
	$(subst $(SPACE),$(COMMA) ,$(sort					\
		$(foreach HSFILE,$(call getVar,$(1),HSFILES), 			\
			$(patsubst $(firstword $(subst /, ,$(HSFILE))).%.hs, %,	\
				$(subst /,.,$(HSFILE))				\
			)							\
		)								\
	))

if ENABLE_PKGREG

# For distributors we usually do not want to register the packages using
# ghc-pkg at install time. That is usually left for a post-install action.
# Therefore these actions can be disabled by ./configure --without-pkgreg

if USE_NEW_PKG_FORMAT

install-data-hook :
	$(if $(PKGCONF),if test -f $(PKGCONF); then :; \
	else echo "[]" > $(PKGCONF); fi;)
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	rm -f $(DESTDIR)$(pkglibdir)/$(patsubst lib%.a,%.o,$(pkgname)); \
	$(GHCPKG) $(addprefix --package-conf=,$(PKGCONF)) --auto-ghci-libs \
	update $(DESTDIR)$(pkglibdir)/$(patsubst libHS%.a,%.package.conf,$(pkgname));)

uninstall-hook :
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	  $(GHCPKG) $(addprefix --package-conf=,$(PKGCONF)) \
	  unregister $(patsubst libHS%.a,%,$(pkgname))-$(PACKAGE_VERSION);) \
	$(if $(PKGCONF),if test -f $(PKGCONF); then \
	  if test -n `head $(PKGCONF) | $(GREP) -e "\[\]"`; then \
	  rm -f $(PKGCONF) $(PKGCONF).old; fi; \
	fi)

else !USE_NEW_PKG_FORMAT

install-data-hook :
	$(if $(PKGCONF),if test -f $(PKGCONF); then :; \
	else echo "[]" > $(PKGCONF); fi;)
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	rm -f $(DESTDIR)$(pkglibdir)/$(patsubst lib%.a,%.o,$(pkgname)); \
	$(GHCPKG) $(addprefix --config-file=,$(PKGCONF)) \
	$(GHCPKG_BUILD_GHCI_LIB) --update-package --force \
	--input-file=$(DESTDIR)$(pkglibdir)/$(patsubst libHS%.a,%.pkg,$(pkgname));)

uninstall-hook :
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	  $(GHCPKG) $(addprefix --config-file=,$(PKGCONF)) \
	  --remove-package=$(patsubst libHS%.a,%,$(pkgname));) \
	$(if $(PKGCONF),if test -f $(PKGCONF); then \
	  if test -n `head $(PKGCONF) | $(GREP) -e "\[\]"`; then \
	  rm -f $(PKGCONF) $(PKGCONF).old; fi; \
	fi)

endif !USE_NEW_PKG_FORMAT

else !ENABLE_PKGREG

install-data-hook :
uninstall-hook :

endif !ENABLE_PKGREG

include mk/common.mk
