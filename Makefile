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
	$(MAKE) -Cdemo/unicode $@ 

noinplace :
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cmogul $@ 
	$(MAKE) -Cgtk $@ 

install : all
	$(MAKE) -Cgtk  $@
	$(MAKE) -Cmogul  $@
	$(MAKE) -Cdemo/unicode $@ 

uninstall :
	$(MAKE) -Cdemo/unicode $@ 
	$(MAKE) -Cmogul  $@
	$(MAKE) -Cgtk  $@

clean	: noinplace
ifeq ($(BUILT_IN_C2HS),yes)
	$(MAKE) -Cc2hs $@
endif
	$(MAKE) -Cgtk $@
	$(MAKE) -Cmogul $@
	$(MAKE) -Cdemo/unicode $@ 

distclean : clean
ifeq ($(BUILT_IN_C2HS),yes)
	$(MAKE) -Cc2hs $@
endif
	$(MAKE) -Cgtk $@
	$(MAKE) -Cmogul $@
	$(MAKE) -Cdemo/unicode $@ 

