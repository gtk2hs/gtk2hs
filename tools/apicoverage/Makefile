# system to help check gtk2hs's api coverage

# which vesion of gtk+ to compare against
GTK_MAJOR_VERSION = 2
GTK_MINOR_VERSION = 4
GTK_MICRO_VERSION = 4

GTK_VERSION = $(GTK_MAJOR_VERSION).$(GTK_MINOR_VERSION).$(GTK_MICRO_VERSION)

main: stats

tars/gtk+-$(GTK_VERSION).tar.bz2 :
	wget ftp://ftp.gtk.org/pub/gtk/v2.$(GTK_MINOR_VERSION)/gtk+-$(GTK_VERSION).tar.bz2 \
	  --directory-prefix=tars

gtk.def : tars/gtk+-$(GTK_VERSION).tar.bz2
	tar --file tars/gtk+-$(GTK_VERSION).tar.bz2 -j --get gtk+-$(GTK_VERSION)/gtk/gtk.def -O > gtk.def
	sed -i 's:\(\t\|EXPORTS\)::' gtk.def

GTK_CHS_FILES = $(shell find ../../gtk -name '*.chs')

gtk.coverage : $(GTK_CHS_FILES)
	grep -h 'foreign import ccall \(unsafe \)\?" \?&\?.*"' `find ../../gtk -name '*.hs'` | \
	sed 's:foreign import ccall \(unsafe \)\?" \?&\?\(.*\)".*:\2:' | \
	sort -u | grep '^gtk_' | grep --invert-match '_get_type$$' > gtk.coverage

# We can have (optional) api.ignore files in the gtk directories to have a
# local place to record functions we don't want to bind, that'd stop the
# central one getting huge.
GTK_IGNORE_FILES = $(shell find ../../gtk -name 'api.ignore')

gtk.ignore.combined : gtk.ignore $(GTK_IGNORE_FILES)
	cat $^ > gtk.ignore.combined

gtk.def.filtered : gtk.def exclude gtk.ignore.combined
	./exclude gtk.ignore.combined < gtk.def > gtk.def.filtered

exclude : Exclude.hs
	ghc --make Exclude.hs -o exclude

clean :
	rm -f Exclude.hi Exclude.o exclude
	rm -f gtk.coverage gtk.def.filtered gtk.def gtk.ignore.combined

gtk_total=$(shell wc -l < gtk.def.filtered)
gtk2hs_total=$(shell wc -l < gtk.coverage)

stats : gtk.def.filtered gtk.coverage
	@echo "**** gtk2hs API coverage report (for gtk-$(GTK_VERSION)) ****"
	@echo " total gtk functions    :" $(gtk_total)
	@echo " total gtk2hs functions :" $(gtk2hs_total)
	@echo " difference             :" `echo $(gtk_total) - $(gtk2hs_total) | bc`
	@echo; echo "run 'diff gtk.def.filtered gtk.coverage' to see in detail"

debug:
	@echo GTK_IGNORE_FILES = $(GTK_IGNORE_FILES)
