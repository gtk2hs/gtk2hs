AUTOMAKE_OPTIONS = foreign subdir-objects
SUFFIXES = .chs.cpp .chs .hsc .deps .dep
DEPDIR = $(addsuffix /,@DEPDIR@)
CLEANFILES = $(DEPDIR)*.dep
DISTCLEANFILES = *.precomp

# Before chaning anything on dependency calculation:
# - Note that the initial dependencies are computed when make checks if
#   the include files are up to date.
# - CFLAGS and CPPFLAGS are not package/application specific. This is due
#   to the initial dependency calculation where it is not clear which
#   package a particular file belongs to. But the CPPFLAGS are needed to
#   run .chs.cpp files through the pre-processor.
# - Dependencies between Haskell .hs modules are stored in
#   .deps/<pkg-name>.deps . To calculate these dependencies, c2hs must be
#   run on the .chs and the header file belonging to the package to which
#   the .chs file belongs to. But since the dependencies for a package are
#   stored in a file with the package's name, set the NAME variable to the
#   file name of the target .deps/<pkg-name>.deps and then run c2hs.

CFLAGS = $(filter-out -I%,$(GTK_CFLAGS))
CPPFLAGS = $(filter -I%,$(GTK_CFLAGS))

SOURCEDIRS = gtk/general gtk/glib gtk/pango gtk/treeList gtk/multiline \
	gtk/gdk gtk/abstract gtk/display gtk/entry gtk/misc gtk/multiline \
	gtk/ornaments gtk/scrolling gtk/treeList gtk/selectors gtk/embedding \
	compat gtk/layout gtk/menuComboToolbar gtk/buttons gtk/windows \
	tools/hierarchyGen tools/callbackGen tools/apicoverage

# fixme: this should be in configure.ac:
HSCPP = $(CPP) -x c -traditional-cpp -P

CHSDEPEND = $(srcdir)/mk/chsDepend

HSC = hsc2hs

# all packages and applications
lib_LIBRARIES = gtk/libgtk2hs.a
bin_PROGRAMS = \
	tools/hierarchyGen/TypeGenerator \
	tools/callbackGen/HookGenerator \
	tools/apicoverage/Exclude

# TypeGenerator

# While building the program <name>, set the variable NAME to <name> so
# we can access program-specific variables like <name>_PACKAGEDEPS. The
# following is a hack to prevent automake from assuming that we are overriding
# the tools/hierarchyGen/TypeGenerator goal.
TypeGenerator_NAME = tools/hierarchyGen/TypeGenerator
$(TypeGenerator_NAME) : NAME = tools_hierarchyGen_TypeGenerator

tools_hierarchyGen_TypeGenerator_MAIN = \
  $(tools_hierarchyGen_TypeGenerator_SOURCES)
tools_hierarchyGen_TypeGenerator_PACKAGEDEPS = data
tools_hierarchyGen_TypeGenerator_SOURCES = \
  tools/hierarchyGen/TypeGen.hs
# Fix automake - the subdir-objects option doesn't work here.
am_tools_hierarchyGen_TypeGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_hierarchyGen_TypeGenerator_SOURCES)))

gtk/general/Hierarchy.chs : $(srcdir)/tools/hierarchyGen/hierarchy.list \
			    $(srcdir)/tools/hierarchyGen/TypeGenerator
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator \
        $(srcdir)/tools/hierarchyGen/hierarchy.list \
        $@ $(addprefix --tag=,$(CREATE_TYPES)))

# HookGenerator
HookGenerator_NAME = tools/callbackGen/HookGenerator
$(HookGenerator_NAME) : NAME = tools_callbackGen_HookGenerator

tools_callbackGen_HookGenerator_MAIN = \
  $(tools_callbackGen_HookGenerator_SOURCES)
tools_callbackGen_HookGenerator_PACKAGEDEPS = data
tools_callbackGen_HookGenerator_SOURCES = \
  tools/callbackGen/HookGenerator.hs
am_tools_callbackGen_HookGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_callbackGen_HookGenerator_SOURCES)))


gtk/general/Signal.chs : $(srcdir)/tools/callbackGen/Signal.chs-boot1 \
			 $(srcdir)/tools/callbackGen/Signal.chs-boot2 \
			 $(srcdir)/tools/callbackGen/gtkmarshal.list \
			 $(srcdir)/tools/callbackGen/HookGenerator
	$(strip $(srcdir)/tools/callbackGen/HookGenerator $(MARSHALLDEFS) \
	$(srcdir)/tools/callbackGen/ gtk/general/Signal.chs \
	$(if $(subst yes,,$(BROKENCB)),--broken))

# API Coverage Tool
Exclude_NAME = tools/apicoverage/Exclude
$(Exclude_NAME) : NAME = tools_apicoverage_Exclude

tools_apicoverage_Exclude_MAIN = \
  $(tools_apicoverage_Exclude_SOURCES)
tools_apicoverage_Exclude_PACKAGEDEPS = data
tools_apicoverage_Exclude_SOURCES = \
  tools/apicoverage/Exclude.hs
am_tools_apicoverage_Exclude_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_apicoverage_Exclude_SOURCES)))


# While building lib<name>, set the variable NAME to <name> so we can access
# the package-specific variable <name>_HEADER, <name>_PACKAGE, etc. The
# following is a hack to prevent automake from assuming that we are overriding
# the libgtk2hs.a goal.
gtk_libgtk2hs_a_NAME = gtk/libgtk2hs.a
$(gtk_libgtk2hs_a_NAME) : NAME = gtk_libgtk2hs_a

gtk_libgtk2hs_a_PACKAGECONF	= libgtk2hs.conf
gtk_libgtk2hs_a_PACKAGE		= gtk2hs
gtk_libgtk2hs_a_PACKAGEDEPS	= data
gtk_libgtk2hs_a_HEADER 		= gtk/gtk.h
gtk_libgtk2hs_a_PRECOMP		= gtk/gtk.precomp
gtk_libgtk2hs_a_LIBS		= @GTK_LIBS@
gtk_libgtk2hs_a_HCFLAGS		= -fglasgow-exts

gtk_libgtk2hs_a_SOURCES = \
	gtk/general/Hierarchy.chs \
	gtk/general/Signal.chs \
	gtk/glib/GValue.chs \
	gtk/glib/GList.chs \
	gtk/glib/GObject.chspp \
	gtk/pango/PangoTypes.chspp \
	gtk/treeList/TreeModel.chspp \
	gtk/treeList/TreeViewColumn.chs \
	gtk/multiline/TextIter.chspp \
	gtk/gdk/Region.chspp \
	gtk/abstract/Bin.chs \
	gtk/abstract/Box.chs \
	gtk/abstract/ButtonBox.chspp \
	gtk/abstract/Container.chs \
	gtk/abstract/FileChooser.chs \
	gtk/abstract/Misc.chs \
	gtk/abstract/Object.chspp \
	gtk/abstract/Paned.chs \
	gtk/abstract/Range.chs \
	gtk/abstract/Scale.chs \
	gtk/abstract/Widget.chs \
	gtk/buttons/Button.chspp \
	gtk/buttons/CheckButton.chs \
	gtk/buttons/RadioButton.chs \
	gtk/buttons/ToggleButton.chs \
	gtk/display/AccelLabel.chs \
	gtk/display/Image.chs \
	gtk/display/Label.chs \
	gtk/display/ProgressBar.chs \
	gtk/display/Statusbar.chs \
	gtk/entry/Editable.chs \
	gtk/entry/Entry.chspp \
	gtk/entry/EntryCompletion.chspp \
	gtk/entry/HScale.chs \
	gtk/entry/SpinButton.chs \
	gtk/entry/VScale.chs \
	gtk/general/Enums.chspp \
	gtk/general/General.chs \
	gtk/general/IconFactory.chspp \
	gtk/general/Style.chs \
	gtk/layout/Alignment.chspp \
	gtk/layout/AspectFrame.chs \
	gtk/layout/Expander.chspp \
	gtk/layout/Fixed.chs \
	gtk/layout/HBox.chs \
	gtk/layout/HButtonBox.chs \
	gtk/layout/HPaned.chs \
	gtk/layout/Layout.chs \
	gtk/layout/Notebook.chspp \
	gtk/layout/Table.chs \
	gtk/layout/VBox.chs \
	gtk/layout/VButtonBox.chs \
	gtk/layout/VPaned.chs \
	gtk/menuComboToolbar/CheckMenuItem.chspp \
	gtk/menuComboToolbar/Combo.chspp \
	gtk/menuComboToolbar/ComboBox.chspp \
	gtk/menuComboToolbar/ComboBoxEntry.chspp \
	gtk/menuComboToolbar/ImageMenuItem.chs \
	gtk/menuComboToolbar/Menu.chspp \
	gtk/menuComboToolbar/MenuBar.chs \
	gtk/menuComboToolbar/MenuItem.chs \
	gtk/menuComboToolbar/MenuShell.chs \
	gtk/menuComboToolbar/OptionMenu.chspp \
	gtk/menuComboToolbar/RadioMenuItem.chs \
	gtk/menuComboToolbar/TearoffMenuItem.chs \
	gtk/menuComboToolbar/ToolItem.chspp \
	gtk/menuComboToolbar/Toolbar.chspp \
	gtk/misc/Adjustment.chs \
	gtk/misc/Calendar.chspp \
	gtk/misc/DrawingArea.chs \
	gtk/misc/EventBox.chspp \
	gtk/misc/FileChooserWidget.chspp \
	gtk/misc/GArrow.chs \
	gtk/misc/HandleBox.chs \
	gtk/misc/SizeGroup.chs \
	gtk/misc/Tooltips.chspp \
	gtk/misc/Viewport.chs \
	gtk/multiline/TextBuffer.chs \
	gtk/multiline/TextMark.chs \
	gtk/multiline/TextTag.chspp \
	gtk/multiline/TextTagTable.chs \
	gtk/multiline/TextView.chs \
	gtk/ornaments/Frame.chs \
	gtk/ornaments/HSeparator.chs \
	gtk/ornaments/VSeparator.chs \
	gtk/scrolling/HScrollbar.chs \
	gtk/scrolling/ScrolledWindow.chs \
	gtk/scrolling/VScrollbar.chs \
	gtk/selectors/ColorSelection.chs \
	gtk/selectors/ColorSelectionDialog.chs \
	gtk/selectors/FontSelection.chs \
	gtk/selectors/FontSelectionDialog.chs \
	gtk/treeList/CellRendererPixbuf.chs \
	gtk/treeList/CellRendererText.chs \
	gtk/treeList/CellRendererToggle.chs \
	gtk/treeList/ListStore.chspp \
	gtk/treeList/TreeModelSort.chs \
	gtk/treeList/TreeSelection.chs \
	gtk/treeList/TreeStore.chspp \
	gtk/treeList/TreeView.chspp \
	gtk/windows/Dialog.chs \
	gtk/windows/FileChooserDialog.chspp \
	gtk/windows/FileSel.chs \
	gtk/windows/Window.chspp \
	gtk/gdk/Drawable.chspp \
	gtk/gdk/GC.chs \
	gtk/gdk/Gdk.chs \
	gtk/gdk/GdkEnums.chs \
	gtk/gdk/Keys.chs \
	gtk/gdk/Pixbuf.chs \
	gtk/glib/GError.chspp \
	gtk/glib/GType.chs \
	gtk/glib/GValueTypes.chs \
	gtk/pango/PangoLayout.chs \
	gtk/pango/Rendering.chs \
	gtk/embedding/Plug.chs \
	gtk/embedding/Socket.chs \
	gtk/general/StockItems.hsc \
	gtk/general/Structs.hsc \
	gtk/treeList/StoreValue.hsc \
	gtk/gdk/Events.hsc \
	gtk/glib/GParameter.hsc \
	gtk/embedding/Embedding.hsc \
	gtk/abstract/Scrollbar.hs \
	gtk/abstract/Separator.hs \
	gtk/general/FFI.hs \
	gtk/general/Gtk.hs \
	gtk/treeList/CellRenderer.hs \
	gtk/gdk/DrawWindow.hs \
	gtk/pango/Markup.hs \
	compat/LocalControl.hs \
	compat/LocalData.hs

am_gtk_libgtk2hs_a_OBJECTS = \
  $(addsuffix .$(OBJEXT),$(basename $(gtk_libgtk2hs_a_SOURCES)))

gtk_libgtk2hs_a_CHSFILES = $(filter %.chs %.chspp, $(gtk_libgtk2hs_a_SOURCES))
gtk_libgtk2hs_a_CHSFILES_HS = $(patsubst %.chs,%.hs,\
	$(patsubst %.chspp,%.hs,$(gtk_libgtk2hs_a_CHSFILES)))
gtk_libgtk2hs_a_HSCFILES = $(filter %.hsc, $(gtk_libgtk2hs_a_SOURCES))
gtk_libgtk2hs_a_HSCFILES_HS = $(gtk_libgtk2hs_a_HSCFILES:.hsc=.hs)
gtk_libgtk2hs_a_BUILDSOURCES = \
	$(gtk_libgtk2hs_a_CHSFILES_HS) \
	$(gtk_libgtk2hs_a_HSCFILES_HS)
gtk_libgtk2hs_a_HSFILES  = \
	$(gtk_libgtk2hs_a_BUILDSOURCES) \
	$(filter %.hs,$(gtk_libgtk2hs_a_SOURCES))
MOSTLYCLEANFILES = $(gtk_libgtk2hs_a_HSFILES:.hs=.$(OBJEXT))
CLEANFILES+= $(gtk_libgtk2hs_a_BUILDSOURCES) $(gtk_libgtk2hs_a_HSFILES:.hs=.hi)
CLEANFILES+= $(DEPDIR)gtk_libgtk2hs_a.deps
CLEANFILES+= $(gtk_libgtk2hs_a_CHSFILES_HS:.hs=.dep)

-include $(DEPDIR)gtk_libgtk2hs_a.deps $(gtk_libgtk2hs_a_CHSFILES_HS:.hs=.dep)

# A file with CPP defines that reflect the current configuration.
CONFIG_H = config.h

# The local GHC package file for compiling files that depend on packages
# that we have built but not yet installed.
LOCALPACKAGE = localpackages.conf

EMPTY =
SPACE = $(EMPTY) $(EMPTY)
VPATH = $(subst $(SPACE),:,$(strip \
	$(if $(subst .,,$(srcdir)),$(addprefix $(srcdir)/,$(SOURCEDIRS)), \
	$(SOURCEDIRS))))

BUILDSOURCES = $(gtk_libgtk2hs_a_BUILDSOURCES)

LINK = $(strip $(HC) -o $@ $($(NAME)_HCFLAGS) \
  $(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
  $(AM_LDFLAGS) $($(NAME)_EXTRA_LDFLAGS) $($(NAME)_LDFLAGS))

.hs.o: $(CONFIG_H)
	$(strip $(HC) -c $< -o $@ $($(NAME)_HCFLAGS) -i$(VPATH) \
	$(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
	$(addprefix -package-name ,$($(NAME)_PACKAGE)) \
	$(addprefix '-\#include<,$(addsuffix >',$(CONFIG_H) \
	$($(NAME)_EXTRA_HFILES))) \
	$(AM_CPPFLAGS) $(EXTRA_CPPFLAGS) $(CPPFLAGS))

.DELETE_ON_ERROR : $(DEPDIR)%.deps

$(DEPDIR)%.deps :
	touch $@
	$(if $($*_BUILDSOURCES),$(strip \
	$(MAKE) $(AM_MAKEFLAGS) NAME="$*" $($*_BUILDSOURCES) \
	&&))\
	$(strip $(HC) -M $(addprefix -optdep,-f $(DEPDIR)$*.deps) \
	$($*_HCFLAGS) -i$(VPATH) \
	$(addprefix -package ,$($*_PACKAGEDEPS)) \
	$(addprefix '-\#include<,$(addsuffix >',$(CONFIG_H) \
	$($*_EXTRA_HFILES))) \
	$(AM_CPPFLAGS) $(EXTRA_CPPFLAGS) $(CPPFLAGS) \
	$($*_HSFILES))

.chs.dep :
	@if test -f $@; then touch $@; else $(CHSDEPEND) -i$(VPATH) $<; fi;

.o.hi:
	@:

HSTOOLFLAGS = -H500m

.PHONY: debug
debug	:
	@echo VPATH: $(VPATH)
	@echo dep files: $(gtk_libgtk2hs_a_CHSFILES_HS:.hs=.dep)

%.precomp :
	$(strip $(C2HS) $(C2HS_FLAGS) +RTS $(HSTOOLFLAGS) -RTS \
	$(addprefix -C,$(CFLAGS) $(CPPFLAGS)) \
	--precomp=$($(NAME)_PRECOMP) $($(NAME)_HEADER))

.chs.cpp.chs: $(CONFIG_H)
	$(strip $(HSCPP) $(AM_CPPFLAGS) \
	$(EXTRA_CPPFLAGS) $(CPPFLAGS) \
	$(EXTRA_CFLAGS) $(CFLAGS) \
	$(addprefix -include ,$(CONFIG_H) $($(NAME)_EXTRA_HFILES)) \
	$< -o $@)

.hsc.hs: $(CONFIG_H)
	$(strip $(HSC) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
        $(addprefix -L-optl,\
	$(AM_LDFLAGS) $($(NAME)_EXTRA_LIBS) $($(NAME)_LIBS)) \
        $(addprefix -C,	$(filter-out -I%,$(AM_CPPFLAGS)) \
	$(EXTRA_CFLAGS) $(CFLAGS))\
        $(filter -I%,$(AM_CPPFLAGS)) \
	$(EXTRA_CPPFLAGS) $(CPPFLAGS)\
	--include $(CONFIG_H) \
        --cc=$(HC) --lflag=-no-hs-main $<)

.chs.hs:
	if test -f $($(NAME)_PRECOMP); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="$(NAME)" $($(NAME)_PRECOMP); fi;
	$(strip $(C2HS) $(C2HS_FLAGS) +RTS $(HSTOOLFLAGS) -RTS \
	-i$(VPATH) --precomp=$($(NAME)_PRECOMP) -o $@ $<)
	$(CHSDEPEND) -i$(VPATH) $<


