TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = sourceview

INSTALLDIR = gtk2hs/sourceview

MAIN = SourceView.hs

# In order for ghc and c2hs to find .chi and .hi files in ../gtk/* we
# need to set these directories here. Note that this list is different
# from SUBDIRS in that the directories given here are not searched for
# compilable files.
HIDIRS = ../gtk/glib ../gtk/abstract ../gtk/general ../gtk/multiline

HEADER = sourceview.h
EXTRA_HFILES	= $(TOP)/sourceview/sourceview.h
EXTRA_TARFILES	= sourceview.h

ifneq (x$(MULTIPLE_CHS),xyes)
SourceIter-HEADER		= gtksourceview/gtksourceiter.h
SourceBuffer-HEADER		= gtksourceview/gtksourcebuffer.h
SourceMarker-HEADER		= gtksourceview/gtksourcemarker.h
SourceView-HEADER		= gtksourceview/gtksourceview.h
SourceLanguage-HEADER		= gtksourceview/gtksourcelanguage.h
SourceLanguagesManager-HEADER	= gtksourceview/gtksourcelanguagesmanager.h
SourceStyleScheme-HEADER	= gtksourceview/gtksourcestylescheme.h
SourceTag-HEADER		= gtksourceview/gtksourcetag.h
SourceTagTable-HEADER		= gtksourceview/gtksourcetagtable.h
SourceTagStyle-HEADER		= gtksourceview/gtksourcetagstyle.h
SourcePringJob-HEADER		= gtksourceview/gtksourceprintjob.h
endif

# Further options to the C preprocessor are passed verbatim:
EXTRA_CPPFLAGS	= $(GTK_CFLAGS)

EXTRA_LIBS	= $(GTK_LIBS)

ifeq ($(strip $(NEW_MODULE_SYS)),yes)
NEEDPACKAGES = gtk2
else
NEEDPACKAGES = gtk2 lang data
endif

EXTRA_CHSFILES	+=  SourceViewType.chs
TYPE_TAGS += sourceview
EXTRA_CLEANFILES+= SourceViewType.chs
NEEDCHI = SourceViewType

SourceViewType.chs : ../tools/typehier/hierarchy.list
	$(strip ../tools/typehier/TypeGenerator \
	../tools/typehier/hierarchy.list \
	SourceViewType.chs \
	$(addprefix --tag=,$(TYPE_TAGS)))


include $(TOP)/mk/common.mk
