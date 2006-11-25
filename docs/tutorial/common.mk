
TOOLSDIR = $(TOP)/../tools

hi-files = \
	$(TOP)/../../gtk/Graphics/UI/Gtk.hi		\
	$(TOP)/../../glade/Graphics/UI/Gtk/Glade.hi
#add the other package's top level .hi files as necessary

$(TOP)/exports : $(hi-files)
	rm -f $@
	for hifile in $^; do \
		$(HC) --show-iface $$hifile \
			| grep '^export' \
			| sed 's/^export [a-zA-Z]*-[0-9.]*:/export /' \
			| tr '{};|' '   ' \
			>> $@; \
	done

%.tex : %.t2t $(TOP)/exports $(TOOLSDIR)/AddLinks
	txt2tags --target=tex --infile=$< --outfile=- \
	| $(TOOLSDIR)/AddLinks $(TOP)/exports --target=tex --baseurl=$(base-url) > $@

%.html : %.t2t $(TOP)/exports $(TOOLSDIR)/AddLinks
	if [ -f $(TOP)/headder.html.fragment -a -f $(TOP)/footer.html.fragment ]; then \
	  ( cat $(TOP)/headder.html.fragment; \
	    txt2tags --target=xhtml --no-headers --infile=$< --outfile=- \
	    | $(TOOLSDIR)/AddLinks $(TOP)/exports --target=xhtml \
		--baseurl=$(base-url); \
	    cat $(TOP)/footer.html.fragment \
	  ) > $@; \
	else \
	  txt2tags --target=xhtml --css-sugar --infile=$< --outfile=- \
	  | $(TOOLSDIR)/AddLinks $(TOP)/exports --target=xhtml > $@; \
	fi

%.dvi : %.tex
	latex $<

%.pdf : %.tex
	pdflatex $<

base-url = http://haskell.org/gtk2hs/docs/current/

$(TOOLSDIR)/AddLinks : $(TOOLSDIR)/AddLinks.hs
	$(HC) $< -o $@

HC=ghc

clean :
	rm -f *.html *.tex *.log *.aux *.out *.dvi *.pdf *.ps
	rm -f $(EXTRA_CLEANFILES)
