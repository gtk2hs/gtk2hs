TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = glade

# Choose a name for the library that is not libglade.a which is the 
# C library.
LIBNAME = glade2hs

MAIN = Glade.hs

# In order for ghc and c2hs to find .chi and .hi files in $(TOP)/gtk/* we
# need to set these directories here. Note that this list is different
# from SUBDIRS in that the directories given here are not searched for
# compilable files.
HIDIRS = $(TOP)/gtk/glib $(TOP)/gtk/abstract $(TOP)/gtk/general

HEADER = glade/glade.h
#EXTRA_HFILES	= 
#EXTRA_TARFILES	= 

ifneq (x$(MULTIPLE_CHS),xyes)
Glade-HEADER		= glade/glade-init.h
GladeXML-HEADER		= glade/glade-xml.h
endif

# Further options to the C preprocessor are passed verbatim:
EXTRA_CPPFLAGS	= $(LIBGLADE_CFLAGS)

EXTRA_LIBS	= $(LIBGLADE_LIBS)

ifeq ($(strip $(NEW_MODULE_SYS)),yes)
NEEDPACKAGES = gtk2
else
NEEDPACKAGES = gtk2 lang data
endif

EXTRA_CHSFILES	+=  GladeType.chs
TYPE_TAGS += libglade
EXTRA_CLEANFILES+= GladeType.chs
NEEDCHI = GladeType

GladeType.chs : $(TOP)/tools/hierarchyGen/hierarchy.list
	$(strip $(TOP)/tools/hierarchyGen/TypeGenerator \
	$(TOP)/tools/hierarchyGen/hierarchy.list \
	GladeType.chs $(addprefix --tag=,$(TYPE_TAGS))) --lib=glade --prefix=glade


include $(TOP)/mk/common.mk
