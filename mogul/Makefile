TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = mogul

MAIN = Mogul.hs

NEEDPACKAGES = gtk2

ifeq ($(DISABLE_DEPRECATED),yes)
EXTRAHC_FLAGS += -DDISABLE_DEPRECATED
endif

include $(TOP)/mk/common.mk
