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
ifeq ($(BUILDDOCS),yes)
	$(MAKE) -Cdoc all
endif

noinplace :
	$(MAKE) -Cmogul $@ 
	$(MAKE) -Cgtk $@ 
	$(MAKE) -Cdemo/treeList $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/hello $@ 

install : all
	$(MAKE) -Cgtk  $@
	$(MAKE) -Cmogul  $@
	$(MAKE) -Cdemo/hello $@ 
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cdemo/graphic $@ 
	$(MAKE) -Cdemo/treeList $@ 

uninstall :
	$(MAKE) -Cmogul  $@
	$(MAKE) -Cgtk  $@
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

EXTRA_TARFILES = $(strip AUTHORS COPYING.LIB ChangeLog INSTALL Makefile \
			 TODO VERSION aclocal.m4 configure.in configure \
			 mk/config.mk.in mk/common.mk mk/application.mk \
		 	 mk/library.mk mk/chsDepend.in install-sh \
			 config.sub config.guess)

dist :
	$(RM) $(TARNAME) $(TARNAME).tar $(TARNAME).tar.gz
	$(LN) -s . $(TARNAME)
	$(strip $(TAR) cf $(addsuffix .tar,$(TARNAME)) \
	  $(addprefix $(TARNAME)/,$(EXTRA_TARFILES)))
	$(MAKE) -Cc2hs tarsource
	$(MAKE) -Cgtk tarsource
	$(MAKE) -Cmogul tarsource
	$(MAKE) -Cdemo/hello tarsource
	$(MAKE) -Cdemo/unicode tarsource
	$(MAKE) -Cdemo/graphic tarsource
	$(MAKE) -Cdemo/treeList tarsource
	$(GZIP) $(TARNAME).tar
	$(RM) $(TARNAME)

