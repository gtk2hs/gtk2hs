TOP = .

include $(TOP)/mk/config.mk

MAKE_TOOLS = tools/hierarchyGen tools/callbackGen

ifeq ($(strip $(BUILT_IN_C2HS)),no)
MAKE_VERB  += c2hs
else
MAKE_TOOLS += c2hs
endif

ifeq ($(strip $(BUILDDOCS)),no)
MAKE_VERB  =  gendoc doc
else
MAKE_DOCS  =  gendoc doc
endif

MAKE_LIBS  = gtk 

ifeq ($(strip $(ENABLE_GNOME)),yes)
MAKE_LIBS += sourceview glade
endif

MAKE_LIBS += mogul

MAKE_APPS  = demo/concurrent demo/treeList demo/graphic demo/unicode \
	     demo/hello demo/buttonbox

ifeq ($(strip $(ENABLE_GNOME)),yes)
MAKE_APPS += demo/sourceview demo/glade
endif

EXTRA_TARFILES = $(strip AUTHORS COPYING.LIB ChangeLog INSTALL Makefile \
			 TODO VERSION aclocal.m4 acinclude.m4 \
			 configure.in configure mk/recurse.mk \
			 mk/config.mk.in mk/common.mk mk/application.mk \
		 	 mk/library.mk mk/chsDepend.in install-sh \
			 config.sub config.guess gtk2hs.spec.in gtk2hs.spec )

dist : configure gtk2hs.spec
	$(RM) -r $(TARNAME)
	$(RM) $(TARNAME).tar $(TARNAME).tar.gz
	$(LN) . $(TARNAME)
	$(strip $(TAR) cf $(addsuffix .tar,$(TARNAME)) \
	  $(addprefix $(TARNAME)/,$(EXTRA_TARFILES)))
	$(MAKE) tarsource
	$(GZIP) $(TARNAME).tar
	$(RM) $(TARNAME)

rpm: dist
	rpmbuild -ba gtk2hs.spec $(RPMOPTS)

srpm: dist
	rpmbuild -bs gtk2hs.spec

%: %.in
	./configure

gtk2hs.spec: VERSION

include $(TOP)/mk/recurse.mk

-include $(TOP)/Makefile.local
