TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = glade

# Choose a name for the library that is not libglade.a which is the 
# C library.
LIBNAME = glade2hs

MAIN = Glade.hs

# help c2hs to find Hierarchy
C2HS_EXTRA_FLAGS = -i$(TOP)/gtk/general

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

NEEDPACKAGES = gtk2

EXTRA_CHSFILES	+=  GladeType.chs
TYPE_TAGS += libglade
EXTRA_CLEANFILES+= GladeType.chs
NEEDCHI = GladeType

GladeType.chs : $(TOP)/tools/hierarchyGen/hierarchy.list
	$(strip $(TOP)/tools/hierarchyGen/TypeGenerator \
	$(TOP)/tools/hierarchyGen/hierarchy.list \
	GladeType.chs $(addprefix --tag=,$(TYPE_TAGS))) --lib=glade --prefix=glade


include $(TOP)/mk/common.mk
