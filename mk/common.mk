# --*Makefile*--

# A file with CPP "defines" that reflect the current configuration.
CONFIG_H = config.h

EMPTY =
SPACE = $(EMPTY) $(EMPTY)
VPATH = $(subst $(SPACE),:,$(strip \
	$(if $(subst .,,$(srcdir)),$(addprefix $(srcdir)/,$(SOURCEDIRS)), \
	$(SOURCEDIRS))))

LINK = 	$(strip $(HC) -o $@ $($(NAME)_HCFLAGS) \
  $(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
  $(AM_LDFLAGS) $($(NAME)_EXTRA_LDFLAGS) $($(NAME)_LDFLAGS))

.hs.o: $(CONFIG_H)
	$(strip $(HC) -c $< -o $@ $($(NAME)_HCFLAGS) -i$(VPATH) \
	$(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
	$(addprefix -package-name ,$($(NAME)_PACKAGE)) \
	$(addprefix '-\#include<,$(addsuffix >',$(CONFIG_H) \
	$($(NAME)_HEADER))) \
	$(AM_CPPFLAGS) $(EXTRA_CPPFLAGS) $(CPPFLAGS))

.DELETE_ON_ERROR : %.deps

%.deps :
	touch $@
	$(if $($*_BUILDSOURCES),$(strip \
	$(MAKE) $(AM_MAKEFLAGS) NAME="$*" $($*_BUILDSOURCES) \
	&&))\
	$(strip $(HC) -M $(addprefix -optdep,-f $*.deps) \
	$($*_HCFLAGS) -i$(VPATH) \
	$(addprefix -package ,$($*_PACKAGEDEPS)) \
	$(addprefix '-\#include<,$(addsuffix >',$(CONFIG_H) \
	$($*_HEADER))) \
	$(AM_CPPFLAGS) $(EXTRA_CPPFLAGS) $(CPPFLAGS) \
	$($*_HSFILES))

.chs.dep :
	@if test -f $@; then touch $@; else $(CHSDEPEND) -i$(VPATH) $<; fi;


.o.hi:
	@:

HSTOOLFLAGS = -H500m

.PHONY: debug
debug	:
	@echo VPATH: $(VPATH)
	@echo dep files: $(gtk_libgtk2hs_a_CHSFILES_HS:.hs=.dep)

%.precomp :
	$(strip $(C2HS) $(C2HS_FLAGS)		\
	+RTS $(HSTOOLFLAGS) $(PROFFLAGS) -RTS		\
	$(addprefix -C,$(CFLAGS) $(CPPFLAGS))		\
	--precomp=$($(NAME)_PRECOMP) $($(NAME)_HEADER))

.chs.pp.chs: $(CONFIG_H)
	$(strip $(HSCPP) $(AM_CPPFLAGS) \
	$(EXTRA_CPPFLAGS) $(CPPFLAGS) \
	$(EXTRA_CFLAGS) $(CFLAGS) \
	$(addprefix -include ,$(CONFIG_H) $($(NAME)_EXTRA_HFILES)) \
	$< -o $@)

.hsc.hs: $(CONFIG_H)
	$(strip $(HSC) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
        $(addprefix -L-optl,\
	$(AM_LDFLAGS) $($(NAME)_EXTRA_LIBS) $($(NAME)_LIBS)) \
        $(addprefix -C,	$(filter-out -I%,$(AM_CPPFLAGS)) \
	$(EXTRA_CFLAGS) $(CFLAGS))\
        $(filter -I%,$(AM_CPPFLAGS)) \
	$(EXTRA_CPPFLAGS) $(CPPFLAGS)\
	--include $(CONFIG_H) \
        --cc=$(HC) --lflag=-no-hs-main $<)

.chs.hs: 
	$(if $(subst no,,$(BUILT_IN_C2HS)),$(strip \
	if test -x $(C2HS); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="tools_c2hs_c2hsLocal" \
	  tools/c2hs/c2hsLocal; fi;))
	$(strip if test -f $($(NAME)_PRECOMP); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="$(NAME)" $($(NAME)_PRECOMP); fi;)
	$(strip $(C2HS) $(C2HS_FLAGS) \
	+RTS $(HSTOOLFLAGS) -RTS \
	-i$(VPATH) --precomp=$($(NAME)_PRECOMP) -o $@ $<)
	$(CHSDEPEND) -i$(VPATH) $<


