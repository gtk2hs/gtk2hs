TOP = ..

include $(TOP)/mk/config.mk

PACKAGENAME = mogul

MAIN = Mogul.hs

NEEDPACKAGES = gtk2

PREPROC_DOCS = GetWidget.hs Mogul.hs NewWidget.hs
HADDOCK_EXTRA_FLAGS = -i../gtk/doc,../gtk/doc/gtk2.haddock

ifeq ($(DISABLE_DEPRECATED),yes)
EXTRAHC_FLAGS += -DDISABLE_DEPRECATED
endif

include $(TOP)/mk/common.mk
