TOP = .

include $(TOP)/mk/config.mk

all	: inplace

inplace : noinplace
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
	$(MAKE) -Cgtk $@
	$(MAKE) -Cmogul $@
	$(MAKE) -Cdemo/unicode $@ 

distclean : clean
	$(MAKE) -Cgtk $@
	$(MAKE) -Cmogul $@
	$(MAKE) -Cdemo/unicode $@ 

