TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = mogul

MAIN = Mogul.hs

ifeq ($(strip $(NEW_MODULE_SYS)),yes)
NEEDPACKAGES = gtk2
else
NEEDPACKAGES = gtk2 data
endif

include $(TOP)/mk/common.mk
