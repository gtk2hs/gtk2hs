AUTOMAKE_OPTIONS = foreign subdir-objects

SUFFIXES = .chspp .chs .hsc

SOURCEDIRS = gtk/general gtk/glib gtk/pango gtk/treeList gtk/multiline \
	gtk/gdk gtk/abstract gtk/display gtk/entry gtk/misc gtk/multiline \
	gtk/ornaments gtk/scrolling gtk/treeList gtk/selectors gtk/embedding \
	compat gtk/layout gtk/menuComboToolbar gtk/buttons gtk/windows

# fixme: this should be in configure.ac:
HSCPP = $(CPP) -x c -traditional-cpp -P

CHSDEPEND = $(srcdir)/mk/chsDepend

HSC = hsc2hs

# Flags for the C compiler and C pre-processor.
# *_CFLAGS variables contain general flags for the C compiler. A subset of
# these, namely just the -I flags, are always available in *_CPPFLAGS.
# Breaking this convention is the automake built-in AM_CPPFLAGS to which
# no AM_CFLAGS exit.

# While building lib<name>, set the variable NAME to <name> so we can access
# the package-specific variable <name>_HEADER, <name>_PACKAGE, etc. The
# following is a hack to prevent automake from assuming that we are overriding
# the libgtk2hs.a goal.
libgtk2hs_a_NAME = libgtk2hs.a
$(libgtk2hs_a_NAME) : NAME = libgtk2hs_a

libgtk2hs_a_TOPLEVEL 	= gtk/general/Gtk.hs
libgtk2hs_a_PACKAGECONF	= libgtk2hs_a.conf
libgtk2hs_a_PACKAGE	= gtk2hs
libgtk2hs_a_PACKAGEDEPS	= data
libgtk2hs_a_HEADER 	= gtk/gtk.h
libgtk2hs_a_PRECOMP	= gtk.precomp
libgtk2hs_a_CFLAGS 	= $(filter-out -I%,@GTK_CFLAGS@)
libgtk2hs_a_CPPFLAGS	= $(filter -I%,@GTK_CFLAGS@)
libgtk2hs_a_LIBS	= @GTK_LIBS@
libgtk2hs_a_HCFLAGS	= -fglasgow-exts

lib_LIBRARIES = libgtk2hs.a

libgtk2hs_a_SOURCES = \
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

am_libgtk2hs_a_OBJECTS = \
  $(addsuffix .$(OBJEXT),$(basename $(libgtk2hs_a_SOURCES)))

libgtk2hs_a_CHSFILES = $(filter %.chs %.chspp, $(libgtk2hs_a_SOURCES))
libgtk2hs_a_CHSFILES_HS = $(patsubst %.chs,%.hs,\
	$(patsubst %.chspp,%.hs,$(libgtk2hs_a_CHSFILES)))
libgtk2hs_a_HSCFILES = $(filter %.hsc, $(libgtk2hs_a_SOURCES))
libgtk2hs_a_BUILDSOURCES = \
	$(libgtk2hs_a_CHSFILES_HS) \
	$(libgtk2hs_a_HSCFILES:.hsc=.hs)
libgtk2hs_a_HSFILES  = \
	$(libgtk2hs_a_BUILDSOURCES) \
	$(filter %.hs,$(libgtk2hs_a_SOURCES))




CONFIG_H = config.h

EMPTY =
SPACE = $(EMPTY) $(EMPTY)
VPATH = $(subst $(SPACE),:,$(strip \
	$(if $(subst .,,$(srcdir)),$(addprefix $(srcdir)/,$(SOURCEDIRS)), \
	$(SOURCEDIRS))))

BUILDSOURCES = $(libgtk2hs_a_BUILDSOURCES)

.hs.o: $(CONFIG_H)
	if test -f .depend; then \
	$(strip $(HC) -c $< -o $@ $($(NAME)_HCFLAGS) -i$(VPATH) \
	  $(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
	  $(addprefix -package-name ,$($(NAME)_PACKAGE)) \
	  $(addprefix '-\#include<,$(addsuffix >',$(CONFIG_H) \
	  $($(NAME)_EXTRA_HFILES))) \
	  $(AM_CPPFLAGS) $($(NAME)_EXTRA_CPPFLAGS) $($(NAME)_CPPFLAGS))\
	; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="$(NAME)" $($(NAME)_BUILDSOURCES) \
	  && \
	  $(strip $(HC) -M $(addprefix -optdep,-f .depend) \
	  $($(NAME)_HCFLAGS) -i$(VPATH) \
	  $(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
	  $(addprefix '-\#include<,$(addsuffix >',$(CONFIG_H) \
	  $($(NAME)_EXTRA_HFILES))) \
	  $(AM_CPPFLAGS) $($(NAME)_EXTRA_CPPFLAGS) $($(NAME)_CPPFLAGS) \
	  $($(NAME)_HSFILES)) \
	; fi

.o.hi:
	@:

HSTOOLFLAGS = -H500m

.PHONY: debug
debug	:
	@echo VPATH: $(VPATH)

%.precomp :
	$(strip $(C2HS) $(C2HS_FLAGS) +RTS $(HSTOOLFLAGS) -RTS \
	$(addprefix -C,$($(NAME)_CFLAGS) $($(NAME)_CPPFLAGS)) \
	--precomp=$($(NAME)_PRECOMP) $($(NAME)_HEADER))

.chspp.chs: $(CONFIG_H)
	$(strip $(HSCPP) $(AM_CPPFLAGS) \
	$($(NAME)_EXTRA_CPPFLAGS) $($(NAME)_CPPFLAGS) \
	$($(NAME)_EXTRA_CFLAGS) $($(NAME)_CFLAGS) \
	$(addprefix -include ,$(CONFIG_H) $($(NAME)_EXTRA_HFILES)) \
	$< -o $@)

.hsc.hs:
	$(strip $(HSC) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
        $(addprefix -L-optl,\
	$(AM_LDFLAGS) $($(NAME)_EXTRA_LIBS) $($(NAME)_LIBS)) \
        $(addprefix -C,	$(filter-out -I%,$(AM_CPPFLAGS)) \
	$($(NAME)_EXTRA_CFLAGS) $($(NAME)_CFLAGS))\
        $(filter -I%,$(AM_CPPFLAGS)) \
	$($(NAME)_EXTRA_CPPFLAGS) $($(NAME)_CPPFLAGS)\
        --cc=$(HC) --lflag=-no-hs-main $<)

.chs.hs:
	if test -f $($(NAME)_PRECOMP); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="$(NAME)" $($(NAME)_PRECOMP); fi;
	$(strip $(C2HS) $(C2HS_FLAGS) +RTS $(HSTOOLFLAGS) -RTS \
	-i$(VPATH) --precomp=$($(NAME)_PRECOMP) -o $@ $<)
	$(CHSDEPEND) -i$(VPATH) $<

-include .depend *.dep


#	$(ECHO) no header file associated with $@
#	exit 1
#
#$(libgtk2hs_a_CHSFILES:.chs=.hs) : %.hs : %.chs $(libgtk2hs_a_PRECOMP)

