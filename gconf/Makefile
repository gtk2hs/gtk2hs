TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = gconf

MAIN = System/Gnome/GConf.hs

# HACK: need to have . is SUBDIRS so that ghc package file works
SUBDIRS = ./ System/Gnome System/Gnome/GConf

# help c2hs find Hierarchy
C2HS_EXTRA_FLAGS = -i$(TOP)/gtk/general

HEADER = gconf/gconf-client.h
#EXTRA_HFILES	= 
#EXTRA_TARFILES	= 

ifneq (x$(MULTIPLE_CHS),xyes)
GConfClient-HEADER		= gconf/gconf-client.h
GConfValue-HEADER		= gconf/gconf-value.h
endif

# required because [a] & String overlap in GConfValue.chs
# They don't actually overlap so it's safe.
EXTRAHC_FLAGS = -fallow-overlapping-instances

# Further options to the C preprocessor are passed verbatim:
EXTRA_CPPFLAGS	= $(GCONF_CFLAGS)

EXTRA_LIBS	= $(GCONF_LIBS)

NEEDPACKAGES = gtk2

EXTRA_CHSFILES	+=  System/Gnome/GConf/GConfType.chs
TYPE_TAGS += gconf
EXTRA_CLEANFILES+= System/Gnome/GConf/GConfType.chs
NEEDCHI = System/Gnome/GConf/GConfType System/Gnome/GConf/GConfValue

System/Gnome/GConf/GConfType.chs : $(TOP)/tools/hierarchyGen/hierarchy.list
	$(strip $(TOP)/tools/hierarchyGen/TypeGenerator \
	$(TOP)/tools/hierarchyGen/hierarchy.list \
	System/Gnome/GConf/GConfType.chs \
	$(addprefix --tag=,$(TYPE_TAGS))) --lib=gconf --prefix=gconf \
	--modname=System.Gnome.GConf.GConfType

include $(TOP)/mk/common.mk

