# system to help check gtk2hs's api coverage

# which vesion of gtk+ to compare against, eg 2.0.9, 2.2.4, 2.4.13
GTK_MAJOR_VERSION = 2
GTK_MINOR_VERSION = 6
GTK_MICRO_VERSION = 1

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
	grep -h 'foreign import ccall \(safe \|unsafe \)\?" \?&\?.*"' \
		`find ../../gtk -name '*.hs'` | \
	sed 's:foreign import ccall \(safe \|unsafe \)\?" \?&\?\(.*\)".*:\2:' | \
	sort -u | grep '^gtk_' | grep --invert-match '_get_type$$' > gtk.coverage

# We can have (optional) api.ignore files in the gtk directories to have a
# local place to record functions we don't want to bind, that'd stop the
# central one getting huge.
GTK_IGNORE_FILES = $(shell find ../../gtk -name 'api.ignore')

gtk.ignore.combined : gtk.ignore $(GTK_IGNORE_FILES)
	cat $^ > gtk.ignore.combined

gtk.def.filtered : gtk.def exclude gtk.ignore.combined
	./exclude gtk.ignore.combined < gtk.def > gtk.def.filtered

gtk.coverage.filtered : gtk.coverage exclude gtk.ignore.combined
	./exclude gtk.ignore.combined < gtk.coverage > gtk.coverage.filtered

exclude : Exclude.hs
	ghc --make Exclude.hs -o exclude

clean :
	rm -f Exclude.hi Exclude.o exclude
	rm -f gtk.coverage gtk.def.filtered gtk.def gtk.ignore.combined

gtk_total=$(shell wc -l < gtk.def.filtered)
gtk2hs_total=$(shell wc -l < gtk.coverage.filtered)
difference=$(shell diff gtk.def.filtered gtk.coverage.filtered | grep -c '<')

stats : gtk.def.filtered gtk.coverage.filtered
	@echo "**** gtk2hs API coverage report (for gtk-$(GTK_VERSION)) ****"
	@echo " total gtk functions    :" $(gtk_total)
	@echo " total gtk2hs functions :" $(gtk2hs_total)
	@echo " unbound gtk functions  :" $(difference)
	@echo
	@echo "run 'make summary' to get a list of groups of unbound functions"
	@echo " on 'diff gtk.def.filtered gtk.coverage' to see in detail"
	@echo " or 'diff gtk.def.filtered gtk.coverage.filtered' for less noise"

summary : gtk.def.filtered gtk.coverage.filtered
	@echo "**** unbound gtk functions by group ****"
	@diff gtk.def.filtered gtk.coverage.filtered | \
	awk '$$1 == "<" { print $$2 }' | \
	awk -F _ '{ printf("%s_%s\n", $$1, $$2) }' | uniq -c | sort -n

summary3: gtk.def.filtered gtk.coverage.filtered
	@echo "**** unbound gtk functions by group ****"
	@diff gtk.def.filtered gtk.coverage.filtered | \
	awk '$$1 == "<" { print $$2 }' | \
	awk -F _ '{ printf("%s_%s_%s\n", $$1, $$2, $$3) }' | uniq -c | sort -n

debug:
	@echo GTK_IGNORE_FILES = $(GTK_IGNORE_FILES)
