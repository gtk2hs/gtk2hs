TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = mogul

INSTALLDIR = mogul

MAIN = Mogul.hs

ifeq ($(strip $(NEW_MODULE_SYS)),yes)
NEEDPACKAGES = gtk2
else
NEEDPACKAGES = gtk2 lang data
endif

include $(TOP)/mk/common.mk
