TOP = .

include $(TOP)/mk/config.mk

# Making the different subdirectories unfortunately cannot be automated. There
# are several interdependencies that cannot easily be detected:
# - all for e.g. mogul/ requires that gtk/ is locally installed (inplace)
# - install requires that the modules are not locally installed, otherwise
#   gtk-pkg complains that the module is already installed (fix: omit the
#   local package file in those cases)
# - uninstall should remove the packages in reverse order, so that the system
#   is always in a consistent state if something goes wrong
# - sometimes we build the local c2hs, sometimes we don't

all	: inplace

inplace : noinplace
ifeq ($(BUILT_IN_C2HS),yes)
	$(MAKE) -Cc2hs $@
endif
	$(MAKE) -Cgtk $@ 
	$(MAKE)	-Cmogul $@ 
	$(MAKE) -Cdemo/hello $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/treeList $@ 
	$(MAKE) -Cdemo/concurrent $@ 
ifeq ($(BUILDDOCS),yes)
	$(MAKE) -Cdoc all
endif

noinplace :
	$(MAKE) -Cmogul $@ 
	$(MAKE) -Cgtk $@
	$(MAKE) -Cdemo/concurrent $@ 
	$(MAKE) -Cdemo/treeList $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/hello $@ 

install install-without-pkg : all
	$(MAKE) -Cgtk  $@
	$(MAKE) -Cmogul  $@
	$(MAKE) -Cdemo/hello install
	$(MAKE) -Cdemo/unicode install
	$(MAKE) -Cdemo/graphic install
	$(MAKE) -Cdemo/treeList install
	$(MAKE) -Cdemo/concurrent install

uninstall :
	$(MAKE) -Cmogul  $@
	$(MAKE) -Cgtk  $@
	$(MAKE) -Cdemo/concurrent $@ 
	$(MAKE) -Cdemo/treeList $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/hello $@ 

clean	: noinplace
ifeq ($(BUILT_IN_C2HS),yes)
	$(MAKE) -Cc2hs $@
endif
	$(MAKE) -Cgtk $@
	$(MAKE) -Cmogul $@
	$(MAKE) -Cdemo/hello $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/treeList $@ 
	$(MAKE) -Cdemo/concurrent $@ 
ifeq ($(BUILDDOCS),yes)
	$(MAKE) -Cdoc $@
endif


distclean : clean
ifeq ($(BUILT_IN_C2HS),yes)
	$(MAKE) -Cc2hs $@
endif
	$(MAKE) -Cgtk $@
	$(MAKE) -Cmogul $@
	$(MAKE) -Cdemo/hello $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/treeList $@ 
	$(MAKE) -Cdemo/concurrent $@ 

EXTRA_TARFILES = $(strip AUTHORS COPYING.LIB ChangeLog INSTALL Makefile \
			 TODO VERSION aclocal.m4 acinclude.m4 \
			 configure.in configure \
			 mk/config.mk.in mk/common.mk mk/application.mk \
		 	 mk/library.mk mk/chsDepend.in install-sh \
			 config.sub config.guess gtk2hs.spec.in gtk2hs.spec )

dist :
	$(RM) -r $(TARNAME)
	$(RM) $(TARNAME).tar $(TARNAME).tar.gz
	$(LN) . $(TARNAME)
	$(strip $(TAR) cf $(addsuffix .tar,$(TARNAME)) \
	  $(addprefix $(TARNAME)/,$(EXTRA_TARFILES)))
	$(MAKE) -Cc2hs tarsource
	$(MAKE) -Cgtk tarsource
	$(MAKE) -Cmogul tarsource
	$(MAKE) -Cdemo/hello tarsource
	$(MAKE) -Cdemo/unicode tarsource
	$(MAKE) -Cdemo/graphic tarsource
	$(MAKE) -Cdemo/treeList tarsource
	$(MAKE) -Cdemo/concurrent tarsource
	$(MAKE) -Cgendoc tarsource
	$(MAKE) -Cdoc tarsource
	$(GZIP) $(TARNAME).tar
	$(RM) $(TARNAME)

rpm: gtk2hs.spec dist
	rpmbuild -ba gtk2hs.spec

srpm: gtk2hs.spec dist
	rpmbuild -bs gtk2hs.spec

gtk2hs.spec: VERSION gtk2hs.spec.in
	./configure
