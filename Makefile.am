AUTOMAKE_OPTIONS = 1.8 foreign no-dependencies
SUFFIXES = .hs.uncpp .hs.pp .chs.pp .chs .hsc .deps .dep
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
        glib/System/Glib/hsgclosure.h		\
	cairo/COPYRIGHT				\
	cairo/cairo.h				\
	gtk/Graphics/UI/Gtk/ModelView/Gtk2HsStore.h

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
if ENABLE_SVGCAIRO
pkglib_LIBRARIES += libHSsvgcairo.a
endif
pkglib_LIBRARIES +=	\
	libHSgtk.a	\
	libHSsoegtk.a
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
if ENABLE_GTKGLEXT
pkglib_LIBRARIES += libHSgtkglext.a
endif

noinst_PROGRAMS = \
	tools/hierarchyGen/TypeGenerator \
	tools/callbackGen/HookGenerator \
	tools/c2hs/c2hsLocal

pkglibincludedir = $(pkglibdir)/include
pkglibinclude_DATA = gtk2hs-config.h

hidir = $(pkglibdir)/imports

#
#	TypeGenerator Tool
#
################################################################################

# While building the program <name>, set the variable NAME to <name> so
# we can access program-specific variables like <name>_EXTERNALDEPS. The
# following is a hack to prevent automake from assuming that we are overriding
# the tools/hierarchyGen/TypeGenerator goal.
tools_hierarchyGen_PKGNAME = tools_hierarchyGen_TypeGenerator

tools_hierarchyGen_TypeGenerator_MAIN = \
  $(tools_hierarchyGen_TypeGenerator_SOURCES)
tools_hierarchyGen_TypeGenerator_EXTERNALDEPS = base haskell98
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
tools_callbackGen_HookGenerator_EXTERNALDEPS = base haskell98
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
#tools_apicoverage_Exclude_EXTERNALDEPS = base haskell98
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
tools_c2hs_c2hsLocal_EXTERNALDEPS	= base haskell98
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

libHSglib_a_NAME		= glib
libHSglib_a_CONFIG		= glib/glib.$(PKGEXT)
libHSglib_a_EXTERNALDEPS	= base haskell98
libHSglib_a_INTERNALDEPS	= 
libHSglib_a_HEADER 		= glib-object.h
libHSglib_a_PRECOMP		= glib/glib.precomp
libHSglib_a_LIBS		= $(GLIB_LIBS)
libHSglib_a_HCFLAGS		= -fffi
libHSglib_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS))
libHSglib_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS))

libHSglib_a_SOURCESDIRS		= glib

libHSglib_a_SOURCES =	        		\
	glib/System/Glib.hs			\
	glib/System/Glib/FFI.hs.pp		\
	glib/System/Glib/UTFString.hs.pp	\
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
        glib/System/Glib/Flags.hs		\
	glib/System/Glib/MainLoop.chs.pp

htmldoc_HSFILES_HIDDEN =				\
	glib/System/Glib/FFI.hs			\
	glib/System/Glib/Types.hs

if USE_GCLOSUE_SIGNALS_IMPL
libHSglib_a_SOURCES += glib/System/Glib/hsgclosure.c
# make calling an undefined function an error so we can catch changes in the
# GHC RTS API:
glib_System_Glib_hsgclosure_c_CFLAGS = -Werror-implicit-function-declaration
endif

glib_System_Glib_Types_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_GError_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_Attributes_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_Signals_hs_HCFLAGS	= '-\#include"hsgclosure.h"'

libHSglib_a_LIBADD  = 				\
	glib/System/Glib/GObject_stub.o		\
	glib/System/Glib/MainLoop_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSglib_a_LIBADD += glib/System/Glib/Signals_stub.o
endif

am_libHSglib_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSglib_a_SOURCES))))

libHSglib_a_HSPPFILES  = $(filter %.hs.pp, $(libHSglib_a_SOURCES))
libHSglib_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglib_a_SOURCES))
libHSglib_a_CHSFILES = \
  $(filter %.chs,$(libHSglib_a_SOURCES:.chs.pp=.chs))
libHSglib_a_CHSFILES_HS = $(libHSglib_a_CHSFILES:.chs=.hs)
libHSglib_a_HSCFILES    = $(filter %.hsc, $(libHSglib_a_SOURCES))
libHSglib_a_HSCFILES_HS = $(libHSglib_a_HSCFILES:.hsc=.hs)
libHSglib_a_BUILDSOURCES = \
	$(libHSglib_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHSglib_a_HCFLAGS += -split-objs
libHSglib_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSglib_a_AR = $(AR) $(ARFLAGS)
endif

#
#	gtk package
#
################################################################################

gtk_PKGNAME = libHSgtk_a

libHSgtk_a_NAME			= gtk
libHSgtk_a_CONFIG		= gtk/gtk.$(PKGEXT)
libHSgtk_a_EXTERNALDEPS		= base haskell98
libHSgtk_a_INTERNALDEPS		= glib
libHSgtk_a_HEADER 		= gtk/gtk.h
libHSgtk_a_PRECOMP		= gtk/gtk.precomp
libHSgtk_a_LIBS			= $(GLIB_LIBS) $(GTK_LIBS)
libHSgtk_a_HCFLAGS		= -fffi
libHSgtk_a_CFLAGS 		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))
libHSgtk_a_CPPFLAGS 		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))

libHSgtk_a_SOURCESDIRS		= gtk

gtk/libHSgtk_a.deps	: glib/libHSglib_a.deps

if ENABLE_CAIRO
libHSgtk_a_EXTERNALDEPS += mtl
libHSgtk_a_INTERNALDEPS += cairo
gtk/libHSgtk_a.deps	: cairo/libHScairo_a.deps
endif

libHSgtk_a_GENERATEDSOURCES =						\
	gtk/Graphics/UI/Gtk/Types.chs					\
	gtk/Graphics/UI/Gtk/Signals.chs

nodist_libHSgtk_a_SOURCES = $(libHSgtk_a_GENERATEDSOURCES)

libHSgtk_a_SOURCES =					 		\
	gtk/Graphics/UI/Gtk.hs						\
	gtk/Graphics/UI/Gtk/Pango/Types.chs.pp				\
	gtk/Graphics/UI/Gtk/Multiline/Types.chs.pp			\
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
	gtk/Graphics/UI/Gtk/Embedding/Plug.chs.pp			\
	gtk/Graphics/UI/Gtk/Embedding/Socket.chs.pp			\
	gtk/Graphics/UI/Gtk/Embedding/Embedding.hsc			\
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
	gtk/Graphics/UI/Gtk/Misc/Adjustment.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Calendar.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/DrawingArea.chs			\
	gtk/Graphics/UI/Gtk/Misc/EventBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Arrow.chs				\
	gtk/Graphics/UI/Gtk/Misc/HandleBox.chs				\
	gtk/Graphics/UI/Gtk/Misc/SizeGroup.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Tooltips.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Viewport.chs				\
	gtk/Graphics/UI/Gtk/Multiline/TextBuffer.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextIter.chs.pp			\
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
	gtk/Graphics/UI/Gtk/TreeList/CellRenderer.hs			\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererPixbuf.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererText.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererToggle.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/IconView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/ListStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeIter.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModelSort.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreePath.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeRowReference.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeViewColumn.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView.hs				\
	gtk/Graphics/UI/Gtk/ModelView/CellLayout.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/CellRenderer.chs			\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererCombo.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererPixbuf.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererProgress.chs.pp	\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererText.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererToggle.chs		\
	gtk/Graphics/UI/Gtk/ModelView/CellView.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/ComboBox.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/ComboBoxEntry.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CustomStore.chs			\
	gtk/Graphics/UI/Gtk/ModelView/EntryCompletion.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/Gtk2HsStore.c			\
	gtk/Graphics/UI/Gtk/ModelView/IconView.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/ListStore.hs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/Sequence.hs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/TreeModelSort.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/TreeSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/TreeStore.hs			\
	gtk/Graphics/UI/Gtk/ModelView/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/TreeViewColumn.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/Types.chs				\
	gtk/Graphics/UI/Gtk/Windows/AboutDialog.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/Dialog.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/MessageDialog.chs.pp		\
	gtk/Graphics/UI/Gtk/Windows/Window.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/WindowGroup.chs			\
	gtk/Graphics/UI/Gtk/Gdk/Drawable.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/GC.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Gdk.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Enums.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Keys.chs				\
	gtk/Graphics/UI/Gtk/Gdk/PixbufData.hs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Pixbuf.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/Pixmap.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Attributes.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Description.chs			\
	gtk/Graphics/UI/Gtk/Pango/Font.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Enums.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/GlyphStorage.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Context.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Layout.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Rendering.chs.pp			\
	gtk/Graphics/UI/Gtk/General/StockItems.hsc			\
	gtk/Graphics/UI/Gtk/General/Structs.hsc				\
	gtk/Graphics/UI/Gtk/Gdk/Events.hsc				\
	gtk/Graphics/UI/Gtk/Abstract/Scrollbar.hs			\
	gtk/Graphics/UI/Gtk/Abstract/Separator.hs			\
	gtk/Graphics/UI/Gtk/Gdk/DrawWindow.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Markup.hs                             \
        gtk/Graphics/UI/Gtk/Cairo.chs.pp

htmldoc_HSFILES_HIDDEN +=						\
	$(libHSgtk_a_GENERATEDSOURCES:.chs=.hs)			\
	gtk/Graphics/UI/Gtk/Signals.hs				\
	gtk/Graphics/UI/Gtk/Gdk/PixbufData.hs			\
	gtk/Graphics/UI/Gtk/Pango/Description.hs		\
	gtk/Graphics/UI/Gtk/Pango/Types.hs			\
	gtk/Graphics/UI/Gtk/Pango/Attributes.hs			\
	gtk/Graphics/UI/Gtk/Pango/GlyphStorage.hs		\
	gtk/Graphics/UI/Gtk/TreeList/TreePath.hs		\
	gtk/Graphics/UI/Gtk/TreeList/TreeRowReference.hs	\
	gtk/Graphics/UI/Gtk/ModelView/TreePath.hs		\
	gtk/Graphics/UI/Gtk/ModelView/TreeRowReference.hs	\
	gtk/Graphics/UI/Gtk/ModelView/Types.hs			\
	gtk/Graphics/UI/Gtk/Multiline/Types.hs			\
	gtk/Graphics/UI/Gtk/Abstract/ContainerChildProperties.hs

gtk_Graphics_UI_Gtk_hs_HCFLAGS			= -fno-warn-duplicate-exports	#FIXME
gtk_Graphics_UI_Gtk_Gdk_Enums_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Types_hs_HCFLAGS	        = -fglasgow-exts
gtk_Graphics_UI_Gtk_General_Structs_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Events_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_General_StockItems_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Region_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Drawable_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_ModelView_Types_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_PixbufData_hs_HCFLAGS	= -fglasgow-exts

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
        gtk/Graphics/UI/Gtk/ModelView/EntryCompletion_stub.o		\
        gtk/Graphics/UI/Gtk/ModelView/TreeModel_stub.o			\
        gtk/Graphics/UI/Gtk/ModelView/TreeSelection_stub.o		\
        gtk/Graphics/UI/Gtk/ModelView/TreeView_stub.o			\
	gtk/Graphics/UI/Gtk/ModelView/IconView_stub.o			\
	gtk/Graphics/UI/Gtk/ModelView/CustomStore_stub.o			\
	gtk/Graphics/UI/Gtk/ModelView/CellLayout_stub.o			\
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

libHSgtk_a_HSPPFILES    = $(filter %.hs.pp, $(libHSgtk_a_ALLSOURCES))
libHSgtk_a_CHSPPFILES   = $(filter %.chs.pp,$(libHSgtk_a_ALLSOURCES))
libHSgtk_a_CHSFILES     = $(filter %.chs,$(libHSgtk_a_ALLSOURCES:.chs.pp=.chs))
libHSgtk_a_CHSFILES_HS  = $(libHSgtk_a_CHSFILES:.chs=.hs)
libHSgtk_a_HSCFILES     = $(filter %.hsc, $(libHSgtk_a_ALLSOURCES))
libHSgtk_a_HSCFILES_HS  = $(libHSgtk_a_HSCFILES:.hsc=.hs)

libHSgtk_a_BUILDSOURCES = \
	$(libHSgtk_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHSgtk_a_HCFLAGS += -split-objs
libHSgtk_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSgtk_a_AR = $(AR) $(ARFLAGS)
endif

#
#	glade package
#
################################################################################

if ENABLE_LIBGLADE

glade_PKGNAME = libHSglade_a

libHSglade_a_NAME		= glade
libHSglade_a_CONFIG		= glade/glade.$(PKGEXT)
libHSglade_a_EXTERNALDEPS	= base haskell98
libHSglade_a_INTERNALDEPS	= gtk glib
libHSglade_a_HEADER 		= glade/glade.h
libHSglade_a_PRECOMP		= glade/glade.precomp
libHSglade_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(LIBGLADE_LIBS)
libHSglade_a_HCFLAGS		= -fffi
libHSglade_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))
libHSglade_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))

libHSglade_a_SOURCESDIRS	= glade

glade/libHSglade_a.deps	: gtk/libHSgtk_a.deps

libHSglade_a_GENERATEDSOURCES =	\
	glade/Graphics/UI/Gtk/Glade/Types.chs

nodist_libHSglade_a_SOURCES = $(libHSglade_a_GENERATEDSOURCES)

libHSglade_a_SOURCES =	        \
        glade/Graphics/UI/Gtk/Glade.chs

htmldoc_HSFILES_HIDDEN += $(libHSglade_a_GENERATEDSOURCES:.chs=.hs)

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

libHSglade_a_HSPPFILES  = $(filter %.hs.pp, $(libHSglade_a_ALLSOURCES))
libHSglade_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglade_a_ALLSOURCES))
libHSglade_a_CHSFILES = \
  $(filter %.chs,$(libHSglade_a_ALLSOURCES:.chs.pp=.chs))
libHSglade_a_CHSFILES_HS = $(libHSglade_a_CHSFILES:.chs=.hs)
libHSglade_a_HSCFILES = $(filter %.hsc, $(libHSglade_a_ALLSOURCES))
libHSglade_a_HSCFILES_HS = $(libHSglade_a_HSCFILES:.hsc=.hs)
libHSglade_a_BUILDSOURCES = \
	$(libHSglade_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHSglade_a_HCFLAGS += -split-objs
libHSglade_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSglade_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_LIBGLADE

#
#	gconf package
#
################################################################################

if ENABLE_GCONF

gconf_PKGNAME = libHSgconf_a

libHSgconf_a_NAME		= gconf
libHSgconf_a_CONFIG		= gconf/gconf.$(PKGEXT)
libHSgconf_a_EXTERNALDEPS	= base haskell98
libHSgconf_a_INTERNALDEPS	= glib
libHSgconf_a_HEADER 		= gconf/gconf-client.h
libHSgconf_a_PRECOMP		= gconf/gconf.precomp
libHSgconf_a_LIBS		= $(GLIB_LIBS) $(GCONF_LIBS)
libHSgconf_a_HCFLAGS		= -fglasgow-exts -fallow-overlapping-instances
libHSgconf_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GCONF_CFLAGS))
libHSgconf_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GCONF_CFLAGS))

libHSgconf_a_SOURCESDIRS	= gconf

gconf/libHSgconf_a.deps	: glib/libHSglib_a.deps

libHSgconf_a_GENERATEDSOURCES =				\
	gconf/System/Gnome/GConf/Types.chs		\
	gconf/System/Gnome/GConf/Signals.chs

nodist_libHSgconf_a_SOURCES = $(libHSgconf_a_GENERATEDSOURCES)

libHSgconf_a_SOURCES =					\
	gconf/System/Gnome/GConf/GConfValue.chs		\
	gconf/System/Gnome/GConf/GConfClient.chs	\
	gconf/System/Gnome/GConf.hs

htmldoc_HSFILES_HIDDEN += $(libHSgconf_a_GENERATEDSOURCES:.chs=.hs)

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

libHSgconf_a_HSPPFILES  = $(filter %.hs.pp, $(libHSgconf_a_ALLSOURCES))
libHSgconf_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgconf_a_ALLSOURCES))
libHSgconf_a_CHSFILES = \
  $(filter %.chs,$(libHSgconf_a_ALLSOURCES:.chs.pp=.chs))
libHSgconf_a_CHSFILES_HS = $(libHSgconf_a_CHSFILES:.chs=.hs)
libHSgconf_a_HSCFILES = $(filter %.hsc, $(libHSgconf_a_ALLSOURCES))
libHSgconf_a_HSCFILES_HS = $(libHSgconf_a_HSCFILES:.hsc=.hs)
libHSgconf_a_BUILDSOURCES = \
	$(libHSgconf_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHSgconf_a_HCFLAGS += -split-objs
libHSgconf_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSgconf_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_GCONF

#
#	sourceview package
#
################################################################################

if ENABLE_SOURCEVIEW

sourceview_PKGNAME = libHSsourceview_a

libHSsourceview_a_NAME		= sourceview
libHSsourceview_a_CONFIG	= sourceview/sourceview.$(PKGEXT)
libHSsourceview_a_EXTERNALDEPS	= base haskell98
libHSsourceview_a_INTERNALDEPS	= glib gtk
libHSsourceview_a_HEADER 	= sourceview.h
libHSsourceview_a_PRECOMP	= sourceview/sourceview.precomp
libHSsourceview_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(SOURCEVIEW_LIBS)
libHSsourceview_a_HCFLAGS	= -fffi
libHSsourceview_a_CFLAGS	= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS))
libHSsourceview_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS)) -Isourceview

libHSsourceview_a_SOURCESDIRS	= sourceview

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

htmldoc_HSFILES_HIDDEN += $(libHSsourceview_a_GENERATEDSOURCES:.chs=.hs)

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

libHSsourceview_a_HSPPFILES  = $(filter %.hs.pp, $(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_CHSPPFILES = $(filter %.chs.pp,$(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_CHSFILES = \
  $(filter %.chs,$(libHSsourceview_a_ALLSOURCES:.chs.pp=.chs))
libHSsourceview_a_CHSFILES_HS = $(libHSsourceview_a_CHSFILES:.chs=.hs)
libHSsourceview_a_HSCFILES = $(filter %.hsc, $(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_HSCFILES_HS = $(libHSsourceview_a_HSCFILES:.hsc=.hs)
libHSsourceview_a_BUILDSOURCES = \
	$(libHSsourceview_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHSsourceview_a_HCFLAGS += -split-objs
libHSsourceview_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSsourceview_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_SOURCEVIEW

#
#	mozembed package
#
################################################################################

if ENABLE_MOZEMBED

mozembed_PKGNAME = libHSmozembed_a

libHSmozembed_a_NAME		= mozembed
libHSmozembed_a_CONFIG		= mozembed/mozembed.$(PKGEXT)
libHSmozembed_a_EXTERNALDEPS	= base haskell98
libHSmozembed_a_INTERNALDEPS	= glib gtk
libHSmozembed_a_HEADER 		= gtkmozembed.h
libHSmozembed_a_PRECOMP		= mozembed/mozembed.precomp
libHSmozembed_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(MOZEMBED_LIBS)
libHSmozembed_a_HCFLAGS		= -fffi
libHSmozembed_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))
libHSmozembed_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))

libHSmozembed_a_SOURCESDIRS	= mozembed

mozembed/libHSmozembed_a.deps	: gtk/libHSgtk_a.deps

libHSmozembed_a_GENERATEDSOURCES =			\
	mozembed/Graphics/UI/Gtk/MozEmbed/Types.chs

nodist_libHSmozembed_a_SOURCES = $(libHSmozembed_a_GENERATEDSOURCES)

libHSmozembed_a_SOURCES =				\
	mozembed/Graphics/UI/Gtk/MozEmbed.chs.pp

htmldoc_HSFILES_HIDDEN += $(libHSmozembed_a_GENERATEDSOURCES:.chs=.hs)

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

libHSmozembed_a_HSPPFILES  = $(filter %.hs.pp, $(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_CHSPPFILES = $(filter %.chs.pp,$(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_CHSFILES = $(filter %.chs,$(libHSmozembed_a_ALLSOURCES:.chs.pp=.chs))
libHSmozembed_a_CHSFILES_HS = $(libHSmozembed_a_CHSFILES:.chs=.hs)
libHSmozembed_a_HSCFILES = $(filter %.hsc, $(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_HSCFILES_HS = $(libHSmozembed_a_HSCFILES:.hsc=.hs)
libHSmozembed_a_BUILDSOURCES = \
	$(libHSmozembed_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHSmozembed_a_HCFLAGS += -split-objs
libHSmozembed_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSmozembed_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_MOZEMBED

#
#	cairo package
#
################################################################################

if ENABLE_CAIRO

cairo_PKGNAME = libHScairo_a

libHScairo_a_NAME		= cairo
libHScairo_a_CONFIG		= cairo/cairo.$(PKGEXT)
libHScairo_a_EXTERNALDEPS	= base haskell98 mtl
libHScairo_a_INTERNALDEPS	= glib
libHScairo_a_HEADER 		= cairo.h
libHScairo_a_PRECOMP		= cairo/cairo.precomp
libHScairo_a_LIBS		= $(CAIRO_LIBS)
libHScairo_a_HCFLAGS		= -fffi
libHScairo_a_CFLAGS		= $(filter-out -I% -D%,$(CAIRO_CFLAGS))
libHScairo_a_CPPFLAGS		= $(filter -I% -D%,$(CAIRO_CFLAGS))

libHScairo_a_SOURCESDIRS	= cairo

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

cairo_Graphics_Rendering_Cairo_Internal_hs_HCFLAGS = -fglasgow-exts

htmldoc_HSFILES_HIDDEN += 							\
	cairo/Graphics/Rendering/Cairo/Types.hs
htmldoc_HSFILES_EXCLUDE =							\
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

libHScairo_a_HSPPFILES  = $(filter %.hs.pp, $(libHScairo_a_ALLSOURCES))
libHScairo_a_CHSPPFILES = $(filter %.chs.pp,$(libHScairo_a_ALLSOURCES))
libHScairo_a_CHSFILES = \
  $(filter %.chs,$(libHScairo_a_ALLSOURCES:.chs.pp=.chs))
libHScairo_a_CHSFILES_HS = $(libHScairo_a_CHSFILES:.chs=.hs)
libHScairo_a_HSCFILES = $(filter %.hsc, $(libHScairo_a_ALLSOURCES))
libHScairo_a_HSCFILES_HS = $(libHScairo_a_HSCFILES:.hsc=.hs)
libHScairo_a_BUILDSOURCES = \
	$(libHScairo_a_HSPPFILES:.hs.pp=.hs) \
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

if ENABLE_SPLITOBJS
libHScairo_a_HCFLAGS += -split-objs
libHScairo_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHScairo_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_CAIRO

#
#	svgcairo package
#
################################################################################

if ENABLE_SVGCAIRO

svgcairo_PKGNAME = libHSsvgcairo_a

libHSsvgcairo_a_NAME		= svgcairo
libHSsvgcairo_a_CONFIG		= svgcairo/svgcairo.$(PKGEXT)
libHSsvgcairo_a_EXTERNALDEPS	= base haskell98 mtl
libHSsvgcairo_a_INTERNALDEPS	= glib cairo
libHSsvgcairo_a_HEADER 		= svg-cairo.h
libHSsvgcairo_a_PRECOMP		= svgcairo/svgcairo.precomp
libHSsvgcairo_a_LIBS		= $(SVGCAIRO_LIBS)
libHSsvgcairo_a_HCFLAGS		= -fffi
libHSsvgcairo_a_CFLAGS		= $(filter-out -I% -D%,$(CAIRO_CFLAGS) $(SVGCAIRO_CFLAGS))
libHSsvgcairo_a_CPPFLAGS	= $(filter -I% -D%,$(CAIRO_CFLAGS) $(SVGCAIRO_CFLAGS))

libHSsvgcairo_a_SOURCESDIRS	= svgcairo

svgcairo/libHSsvgcairo_a.deps: cairo/libHSsvgcairo_a.deps

libHSsvgcairo_a_GENERATEDSOURCES =

nodist_libHSsvgcairo_a_SOURCES = $(libHSsvgcairo_a_GENERATEDSOURCES)

libHSsvgcairo_a_SOURCES =							\
	svgcairo/Graphics/Rendering/Cairo/SVG.chs

libHSsvgcairo_a_ALLSOURCES = $(libHSsvgcairo_a_SOURCES) $(nodist_libHSsvgcairo_a_SOURCES)

am_libHSsvgcairo_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsvgcairo_a_ALLSOURCES))))

libHSsvgcairo_a_HSPPFILES  = $(filter %.hs.pp, $(libHSsvgcairo_a_ALLSOURCES))
libHSsvgcairo_a_CHSPPFILES = $(filter %.chs.pp,$(libHSsvgcairo_a_ALLSOURCES))
libHSsvgcairo_a_CHSFILES = \
  $(filter %.chs,$(libHSsvgcairo_a_ALLSOURCES:.chs.pp=.chs))
libHSsvgcairo_a_CHSFILES_HS = $(libHSsvgcairo_a_CHSFILES:.chs=.hs)
libHSsvgcairo_a_HSCFILES = $(filter %.hsc, $(libHSsvgcairo_a_ALLSOURCES))
libHSsvgcairo_a_HSCFILES_HS = $(libHSsvgcairo_a_HSCFILES:.hsc=.hs)
libHSsvgcairo_a_BUILDSOURCES = \
	$(libHSsvgcairo_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSsvgcairo_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSsvgcairo_a_CHSFILES_HS) \
        $(libHSsvgcairo_a_HSCFILES_HS) \
        $(libHSsvgcairo_a_GENERATEDSOURCES)
libHSsvgcairo_a_HSFILES  = \
	$(filter %.hs,$(libHSsvgcairo_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsvgcairo_a_ALLSOURCES))

nobase_hi_DATA += $(libHSsvgcairo_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSsvgcairo_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSsvgcairo_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSsvgcairo_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSsvgcairo_a_BUILDSOURCES) 

CLEANFILES+= svgcairo/libHSsvgcairo_a.deps $(libHSsvgcairo_a_CHSFILES_HS:.hs=.dep)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSsvgcairo_a_CHSFILES:.chs=.dep) cairo/libHSsvgcairo_a.deps
 endif

if ENABLE_SPLITOBJS
libHSsvgcairo_a_HCFLAGS += -split-objs
libHSsvgcairo_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSsvgcairo_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_SVGCAIRO

#
#	gtkglext package
#
################################################################################

if ENABLE_GTKGLEXT

gtkglext_PKGNAME = libHSgtkglext_a

libHSgtkglext_a_NAME		= gtkglext
libHSgtkglext_a_CONFIG		= gtkglext/gtkglext.$(PKGEXT)
libHSgtkglext_a_EXTERNALDEPS	= base haskell98
libHSgtkglext_a_INTERNALDEPS	= glib gtk
libHSgtkglext_a_HEADER 		= gtk/gtkgl.h
libHSgtkglext_a_PRECOMP		= gtkglext/gtkglext.precomp
libHSgtkglext_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(GTKGLEXT_LIBS)
libHSgtkglext_a_HCFLAGS		= -fffi
libHSgtkglext_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTKGLEXT_CFLAGS))
libHSgtkglext_a_CPPFLAGS	= $(filter     -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTKGLEXT_CFLAGS))

libHSgtkglext_a_SOURCESDIRS	= gtkglext

gtkglext/libHSgtkglext_a.deps	: gtk/libHSgtk_a.deps

libHSgtkglext_a_GENERATEDSOURCES =					\
	gtkglext/Graphics/UI/Gtk/OpenGL/Types.chs

nodist_libHSgtkglext_a_SOURCES = $(libHSgtkglext_a_GENERATEDSOURCES)

libHSgtkglext_a_SOURCES =						\
	gtkglext/Graphics/UI/Gtk/OpenGL.hs				\
	gtkglext/Graphics/UI/Gtk/OpenGL/Config.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Context.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Drawable.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Pixmap.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Window.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/General.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/DrawingArea.chs

htmldoc_HSFILES_HIDDEN += $(libHSgtkglext_a_GENERATEDSOURCES:.chs=.hs)

gtkglext_Graphics_UI_Gtk_OpenGL_Types_hs_HCFLAGS	= -fglasgow-exts

gtkglext/Graphics/UI/Gtk/OpenGL/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gtkglext --parentname=Graphics.UI.Gtk.Types \
        --modname=Graphics.UI.Gtk.OpenGL.Types)

libHSgtkglext_a_ALLSOURCES = $(libHSgtkglext_a_SOURCES) $(nodist_libHSgtkglext_a_SOURCES)

am_libHSgtkglext_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtkglext_a_ALLSOURCES))))

libHSgtkglext_a_HSPPFILES  = $(filter %.hs.pp, $(libHSgtkglext_a_ALLSOURCES))
libHSgtkglext_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgtkglext_a_ALLSOURCES))
libHSgtkglext_a_CHSFILES = \
  $(filter %.chs,$(libHSgtkglext_a_ALLSOURCES:.chs.pp=.chs))
libHSgtkglext_a_CHSFILES_HS = $(libHSgtkglext_a_CHSFILES:.chs=.hs)
libHSgtkglext_a_HSCFILES = $(filter %.hsc, $(libHSgtkglext_a_ALLSOURCES))
libHSgtkglext_a_HSCFILES_HS = $(libHSgtkglext_a_HSCFILES:.hsc=.hs)
libHSgtkglext_a_BUILDSOURCES = \
	$(libHSgtkglext_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSgtkglext_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtkglext_a_CHSFILES_HS) \
        $(libHSgtkglext_a_HSCFILES_HS) \
        $(libHSgtkglext_a_GENERATEDSOURCES)
libHSgtkglext_a_HSFILES  = \
	$(filter %.hs,$(libHSgtkglext_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtkglext_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgtkglext_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSgtkglext_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSgtkglext_a_HSFILES:.hs=.hi)
MOSTLYCLEANFILES += $(libHSgtkglext_a_CHSFILES:.chs=.chi)
CLEANFILES += $(libHSgtkglext_a_BUILDSOURCES) 

CLEANFILES+= gtkglext/libHSgtkglext_a.deps $(libHSgtkglext_a_CHSFILES_HS:.hs=.dep)

$(libHSgtkglext_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)		\
	$(libHSgtkglext_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgtkglext_a_CHSFILES:.chs=.dep) gtkglext/libHSgtkglext_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgtkglext_a_HCFLAGS += -split-objs
libHSgtkglext_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSgtkglext_a_AR = $(AR) $(ARFLAGS)
endif

endif #ENABLE_GTKGLEXT

#
#	soegtk package
#
################################################################################

soegtk_PKGNAME = libHSsoegtk_a

libHSsoegtk_a_NAME		= soegtk
libHSsoegtk_a_CONFIG		= soegtk/soegtk.$(PKGEXT)
libHSsoegtk_a_EXTERNALDEPS	= base haskell98
libHSsoegtk_a_INTERNALDEPS	= glib gtk
libHSsoegtk_a_HEADER		= $(libHSgtk_a_HEADER)
libHSsoegtk_a_LIBS		= 
libHSsoegtk_a_HCFLAGS		=
libHSsoegtk_a_CFLAGS 		= $(libHSgtk_a_CFLAGS)
libHSsoegtk_a_CPPFLAGS 		= $(libHSgtk_a_CPPFLAGS)

libHSsoegtk_a_SOURCESDIRS	= soegtk

soegtk/libHSsoegtk_a.deps	: gtk/libHSgtk_a.deps

if ENABLE_CAIRO
libHSsoegtk_a_EXTERNALDEPS += mtl
libHSsoegtk_a_INTERNALDEPS += cairo
gtk/libHSsoegtk_a.deps	: cairo/libHScairo_a.deps
endif

libHSsoegtk_a_SOURCES =			\
	soegtk/Graphics/SOE/Gtk.hs.pp

am_libHSsoegtk_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsoegtk_a_SOURCES))))

libHSsoegtk_a_HSPPFILES    = $(filter %.hs.pp, $(libHSsoegtk_a_SOURCES))
libHSsoegtk_a_BUILDSOURCES = \
	$(libHSsoegtk_a_HSPPFILES:.hs.pp=.hs)
libHSsoegtk_a_HSFILES  = \
	$(filter %.hs,$(libHSsoegtk_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsoegtk_a_ALLSOURCES))

nobase_hi_DATA += $(libHSsoegtk_a_HSFILES:.hs=.hi)

MOSTLYCLEANFILES += $(am_libHSsoegtk_a_OBJECTS)
MOSTLYCLEANFILES += $(libHSsoegtk_a_HSFILES:.hs=.hi)

CLEANFILES+= soegtk/libHSsoegtk_a.deps

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include soegtk/libHSsoegtk_a.deps
 endif

if ENABLE_SPLITOBJS
libHSsoegtk_a_HCFLAGS += -split-objs
libHSsoegtk_a_AR = $(srcdir)/mk/link-splitobjs.sh
else
libHSsoegtk_a_AR = $(AR) $(ARFLAGS)
endif

#
#       inplace package registration
#
################################################################################

# As of GHC 6.6 it is no longer possible to build multiple packages without
# registering dependent ones first. So what we have to do now is to create a
# package.conf.inplace package db and register all our packages in it. Then we
# use this package db when building the other packages.

if USE_NEW_PKG_FORMAT

package.conf.inplace:
	echo "[]" > $@
	$(foreach pkg,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$${pkglibdir}/imports|.|' \
		-e 's|"$${pkglibdir}/include"|.|' \
		-e 's|$${pkglibdir}|.|' \
		-e 's|$${modules}|$(call getModules,$(pkg))|' \
		$(call getVar,$(pkg),CONFIG) \
		| $(GHCPKG) update - --force --package-conf=$@; )

else

package.conf.inplace:
	echo "[]" > $@
	$(foreach pkg,$(pkglib_LIBRARIES), \
	$(SED)  -e 's|$${pkglibdir}/imports|.|' \
		-e 's|"$${pkglibdir}/include"|.|' \
		-e 's|$${pkglibdir}|.|' \
		-e 's|$${modules}|$(call getModules,$(pkg))|' \
		$(call getVar,$(pkg),CONFIG) \
		| $(GHCPKG) --update-package --input=- --force | --config-file=$@; )

endif

.DELETE_ON_ERROR: package.conf.inplace

CLEANFILES += package.conf.inplace package.conf.inplace.old

#
#	documentation
#
################################################################################

if BUILDDOCS

# For now just build all the docs together, we can think about doing it in a
# more modular fashion later if it turns out to be useful.

htmldocdir = $(datadir)/doc/gtk2hs/html

PACKAGES = $(foreach PACKAGE,$(pkglib_LIBRARIES), $(patsubst libHS%.a,%,$(PACKAGE)))

htmldoc_HSFILES =  $(filter-out $(htmldoc_HSFILES_EXCLUDE),	\
		$(foreach PACKAGE,$(PACKAGES),$(libHS$(PACKAGE)_a_HSFILES)))

htmldoc_DATA =									  \
	$(foreach HSFILE, $(filter-out						  \
	$(htmldoc_HSFILES_HIDDEN),$(htmldoc_HSFILES)),				  \
	    $(patsubst $(firstword $(subst /,					  \
	    ,$(HSFILE)))-%.hs,							  \
	     docs/reference/%.html,						  \
		$(subst /,-,$(HSFILE:.uncpp=))))				  \
	docs/reference/haddock.css docs/reference/haskell_icon.gif		  \
	docs/reference/minus.gif docs/reference/haddock.js			  \
	docs/reference/index.html docs/reference/doc-index.html			  \
	docs/reference/gtk2hs.haddock						  \
	$(foreach LETTER, A B C D E F G H I J K L M N O P R S T U V W X 58,	  \
		docs/reference/doc-index-$(LETTER).html) #missing letters Q and Z

$(htmldoc_DATA) : $(htmldoc_HSFILES)
	mkdir -p docs/reference
	$(strip $(HADDOCK) --html --odir=docs/reference \
		--title="Gtk2Hs" \
		--dump-interface=docs/reference/gtk2hs.haddock \
		--prologue=docs/prologue.txt \
		$(addprefix --use-package=,$(HADDOCK_PACKAGES)) \
		$(HADDOCK_FLAGS) $^)

CLEANFILES += $(htmldoc_DATA)

docdistdir = $(PACKAGE)-docs-$(VERSION)
dist-docs : $(htmldoc_DATA)
	ln -s docs/reference $(docdistdir)
	$(AMTAR) -c $(docdistdir) -hzf $(docdistdir).tar.gz
	rm $(docdistdir)

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
DEMOS += demo/filechooser
endif
endif
if HAVE_GTK_VERSION_2_6
DEMOS += demo/actionMenu demo/profileviewer
endif
if HAVE_GTK_VERSION_2_8
if ENABLE_CAIRO
DEMOS += demo/cairo
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
if ENABLE_GTKGLEXT
DEMOS += demo/opengl
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
	demo/treeList/ListTest.hs demo/treeList/ListTest2.hs		\
		demo/treeList/SimpleList.hs demo/treeList/TreeTest.hs	\
		demo/treeList/Makefile					\
	demo/unicode/Arabic.hs demo/unicode/Makefile			\
	demo/calc/CalcModel.hs demo/calc/Calc.hs demo/calc/Makefile	\
		demo/calc/calc.glade					\
	demo/fastdraw/FastDraw.hs demo/fastdraw/Makefile		\
	demo/actionMenu/ActionMenu.hs demo/actionMenu/Makefile		\
	demo/cairo/CairoGhci.hs demo/cairo/Clock.hs			\
		demo/cairo/Drawing.hs demo/cairo/Drawing2.hs		\
		demo/cairo/Graph.hs demo/cairo/StarAndRing.hs		\
		demo/cairo/Text.hs demo/cairo/Makefile 			\
		demo/cairo/cairo-clock-icon.png				\
	demo/opengl/RotatingCube.hs demo/opengl/Makefile

#
#       installing and registering packages with the Haskell compiler
#
################################################################################

# Include all the .pkg/.package.conf files in the install set, they're useful
# to distributors and if your installation gets corrupt you can re-register the
# packages from them.
pkglib_DATA = $(foreach pkgname,$(pkglib_LIBRARIES), $(call getVar,$(pkgname),CONFIG))

# Before we install the .pkg/.package.conf files we need to substitute for the
# ${pkglibdir} and ${modules} variables.
if !WIN32_INSTALLER
install-exec-local:
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$${pkglibdir}|$(pkglibdir)|g' \
		-e 's|$${modules}|$(call getModules,$(pkgname))|' \
		$(call getVar,$(pkgname),CONFIG) \
	> $(call getVar,$(pkgname),CONFIG).tmp; \
	mv $(call getVar,$(pkgname),CONFIG).tmp $(call getVar,$(pkgname),CONFIG); \
	$(SED)	's|$${modules}|$(call getModules,$(pkgname))|' \
		$(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),CONFIG)) \
	> $(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),CONFIG)).tmp; \
	mv $(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),CONFIG)).tmp \
	   $(patsubst %.$(PKGEXT),%.cabal,$(call getVar,$(pkgname),CONFIG));)

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
		$(call getVar,$(pkgname),CONFIG) \
	> $(call getVar,$(pkgname),CONFIG).tmp; \
	mv $(call getVar,$(pkgname),CONFIG).tmp $(call getVar,$(pkgname),CONFIG);)
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
	$(GHCPKG) $(addprefix --package-conf=,$(PKGCONF)) $(GHCPKG_BUILD_GHCI_LIB) \
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
