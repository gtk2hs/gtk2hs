TOP = ..
HERE = doc/

include $(TOP)/mk/config.mk

EMPTY	=
SPACE	= $(EMPTY) $(EMPTY)

GENDOC	= gendoc/gendoc

SUBDIRS_GTK 	= gtk/abstract gtk/buttons gtk/display gtk/ gtk/embedding \
		  gtk/entry gtk/general gtk/layout gtk/menuComboToolbar \
		  gtk/misc gtk/multiline gtk/ornaments gtk/scrolling \
		  gtk/treeList gtk/windows gtk/gdk gtk/glib gtk/pango \
		  compat

SUBDIRS_MOGUL 	= mogul $(SUBDIRS_GTK)

TOPLEVEL_GTK 	= Gtk.hs

TOPLEVEL_MOGUL 	= Mogul.hs

EXCLUDE_GTK	= Foreign Monad LocalData Exception IORef LocalControl \
		  Bits UTFCForeign Signal Char Maybe Prelude System

EXCLUDE_MOGUL	= $(EXCLUDE_GTK)

OUTPUT_GTK	= referenceGTK.xml

OUTPUT_MOGUL	= referenceMOGUL.xml

OPTIONS_GTK	=

OPTIONS_MOGUL	=

OUTPUT_FO_GTK	= gtk2hs.fo

OUTPUT_FO_MOGUL	= mogul.fo

INPUT_GTK	= gtk2hs.xml

INPUT_MOGUL	= mogul.xml

.PRECIOUS: $(OUTPUT_GTK) $(OUTPUT_MOGUL)

reference%.xml	: $(TOP)/$(GENDOC)
	$(strip $(TOP)/$(GENDOC) -o $(OUTPUT_$*) \
	  -I$(subst $(SPACE),:,$(strip $(addprefix $(TOP)/,$(SUBDIRS_$*)))) \
	  $(addprefix -x,$(EXCLUDE_$*)) $(OPTIONS_$*) $(TOPLEVEL_$*))

html%	: $(OUTPUT_GTK) $(OUTPUT_MOGUL)
	$(strip SGML_CATALOG_FILES=$(CATALOGS) $(XSLTPROC) --catalogs \
	  -nonet $(XSLTHTML) $(INPUT_$*))
	mv *.html $*

fo%	: $(OUTPUT_$*)
	$(strip SGML_CATALOG_FILES=$(CATALOGS) $(XSLTPROC) --catalogs \
	  -o $(OUTPUT_$*) -nonet $(XSLTFO) $(INPUT_$*))
	mv *.fo $*

.PHONY: $(TOP)/$(GENDOC)

$(TOP)/$(GENDOC) :
	$(MAKE) -C $(TOP)/$(dir $(GENDOC))

all : htmlGTK htmlMOGUL


