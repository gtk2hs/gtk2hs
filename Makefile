TOP = .

include $(TOP)/mk/config.mk

MAKE_TOOLS = c2hs tools/typehier tools/signals

ifeq ($(strip $(BUILDDOCS)),no)
MAKE_VERB  =  gendoc doc
else
MAKE_TOOLS  +=  gendoc doc
endif

MAKE_LIBS  = gtk sourceview mogul

MAKE_APPS  = demo/concurrent demo/treeList demo/graphic demo/unicode \
	     demo/hello

EXTRA_TARFILES = $(strip AUTHORS COPYING.LIB ChangeLog INSTALL Makefile \
			 TODO VERSION aclocal.m4 acinclude.m4 \
			 configure.in configure mk/recurse.mk \
			 mk/config.mk.in mk/common.mk mk/application.mk \
		 	 mk/library.mk mk/chsDepend.in install-sh \
			 config.sub config.guess gtk2hs.spec.in gtk2hs.spec )

dist :
	$(RM) -r $(TARNAME)
	$(RM) $(TARNAME).tar $(TARNAME).tar.gz
	$(LN) . $(TARNAME)
	$(strip $(TAR) cf $(addsuffix .tar,$(TARNAME)) \
	  $(addprefix $(TARNAME)/,$(EXTRA_TARFILES)))
	$(MAKE) tarsource
	$(GZIP) $(TARNAME).tar
	$(RM) $(TARNAME)

rpm: gtk2hs.spec dist
	rpmbuild -ba gtk2hs.spec

srpm: gtk2hs.spec dist
	rpmbuild -bs gtk2hs.spec

gtk2hs.spec: VERSION gtk2hs.spec.in
	./configure

include $(TOP)/mk/recurse.mk
