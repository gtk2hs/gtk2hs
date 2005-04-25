
top = ../..
tools = ../tools

default :
#add the default files to build here, eg: HelloWorld.xhtml HelloWorld.pdf

hi-files = \
	$(top)/gtk/Graphics/UI/Gtk.hi		\
	$(top)/glade/Graphics/UI/Gtk/Glade.hi
#add the other package's top level .hi files as necessary

exports : $(hi-files)
	rm -f $@
	for hifile in $^; do \
		$(HC) --show-iface $$hifile \
			| grep '^\(__\)\?export' \
			| sed 's:^__export:export:' \
			| tr '{};|' '   ' \
			>> exports; \
	done

%.tex : %.t2t exports $(tools)/AddLinks
	txt2tags --target=tex --infile=$< --outfile=- \
	| $(tools)/AddLinks exports --target=tex --baseurl=$(base-url) > $@

%.xhtml : %.t2t exports $(tools)/AddLinks
	txt2tags --target=xhtml --css-sugar --infile=$< --outfile=- \
	| $(tools)/AddLinks exports --target=xhtml --baseurl=$(base-url) > $@

%.dvi : %.tex
	latex $<

%.pdf : %.tex
	pdflatex $<

base-url = http://gtk2hs.sourceforge.net/docs/gtk2hs-docs-0.9.7/

$(tools)/AddLinks : $(tools)/AddLinks.hs
	$(HC) $< -o $@

HC=ghc

clean :
	rm -f $(tools)/AddLinks.o $(tools)/AddLinks.hi $(tools)/AddLinks
	rm -f exports *.xhtml *.tex
	rm -f *.log *.aux *.out *.dvi *.pdf *.ps
