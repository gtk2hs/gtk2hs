# --*Makefile*--

# The variables MAKE_TOOLS, MAKE_LIBS or MAKE_APPS determine on which
# subdirectories the following standard targets work. MAKE_VERB points
# to extra directories that are included when cleaning and tarring.


all : MAKE_GOALS=$(MAKE_TOOLS) $(MAKE_LIBS) $(MAKE_DOCS) $(MAKE_APPS)
all : make-all

inplace : MAKE_GOALS=$(MAKE_TOOLS) $(MAKE_LIBS) $(MAKE_DOCS)
inplace : make-inplace

noinplace : MAKE_GOALS=$(MAKE_TOOLS) $(MAKE_LIBS) $(MAKE_DOCS)
noinplace : make-noinplace

tarsource : MAKE_GOALS=$(MAKE_TOOLS) $(MAKE_LIBS) $(MAKE_DOCS) $(MAKE_APPS) $(MAKE_VERB)
tarsource : make-tarsource

clean : MAKE_GOALS=$(MAKE_TOOLS) $(MAKE_LIBS) $(MAKE_DOCS) $(MAKE_APPS) $(MAKE_VERB)
clean : make-clean

install : MAKE_GOALS=$(MAKE_LIBS) $(MAKE_APPS)
install : make-install

install-without-pkg : MAKE_GOALS=$(MAKE_LIBS) $(MAKE_APPS)
install-without-pkg : make-install-without-pkg

uninstall : MAKE_GOALS=$(MAKE_LIBS) $(MAKE_APPS)
uninstall : make-uninstall

make-% :
	for dir in $(MAKE_GOALS); do $(MAKE) $* -C$$dir ; done;
