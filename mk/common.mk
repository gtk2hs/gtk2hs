# --*Makefile*--

# A file with CPP "defines" that reflect the current configuration.
CONFIG_H = config.h

EMPTY :=
SPACE := $(EMPTY) $(EMPTY)

pkgVPATH = $(subst $(SPACE),:,$($(1)_SOURCESDIRS))

LINK = 	$(strip $(HC) -o $@ $(HCFLAGS) $($(NAME)_HCFLAGS) \
	$(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
	$(AM_LDFLAGS) $($(NAME)_LDFLAGS))

#Using pattern rule here to prevent automake from understanding the rule
#and falsely concluding that two source files will produce the same object
#file even though the object files will be in different directories.
#Obviously the 'subdir-objects' option only works for C/C++ files.
%.o : %.hs $(CONFIG_H)
	@echo Building for $(NAME)
	$(strip $(HC) -c $< -o $@ $(HCFLAGS) $($(NAME)_HCFLAGS) \
	$(call getVar,$<,HCFLAGS) -i$(call pkgVPATH,$(NAME)) \
	$(addprefix -package-name ,$(notdir $(basename $($(NAME)_PACKAGE)))) \
	$(addprefix '-#include<,$(addsuffix >', $($(NAME)_HEADER))) \
	$(AM_CPPFLAGS) $($(NAME)_CPPFLAGS))

.DELETE_ON_ERROR : %.deps

%.deps :
	@echo Checking if deps up to date for $@
	$(strip if test -f $@; then touch $@; else \
	  touch $@; $(MAKE) $(AM_MAKEFLAGS) NAME="$*" depend; fi;)

.PHONY: depend

depend: $($(NAME)_BUILDSOURCES)
	$(if $(word 2,$($(NAME)_HSFILES)),\
	$(HC) -M $(addprefix -optdep,-f $(NAME).deps) \
	$($(NAME)_HCFLAGS) -i$(call pkgVPATH,$(NAME)) \
	$(addprefix -package ,$($(NAME)_PACKAGEDEPS)) \
	$(AM_CPPFLAGS) $(EXTRA_CPPFLAGS) $(CPPFLAGS) \
	$($(NAME)_HSFILES))

.chs.dep :
	@$(CHSDEPEND) -i$(call pkgVPATH,$(NAME)) $<

.hs.chi :
	@:

.o.hi:
	@:

# The cheeky rule for .hi files says that .hi files can be created as
# side-effect of generating a .o file. Make sure the .hi files are not
# deleted as normal intermediate files are.
.PRECIOUS: %.hi

# Same for .chi
.PRECIOUS: %.chi

HSTOOLFLAGS = -H500m -M650m

.PHONY: debug
debug	:
	@echo VPATH: $(VPATH)
	@echo hs files: $(libgtk2hs_a_HSFILES)



%.precomp :
	$(strip $(C2HS) $(C2HS_FLAGS)		\
	+RTS $(HSTOOLFLAGS) $(PROFFLAGS) -RTS		\
	$(addprefix -C,$($(NAME)_CFLAGS) $($(NAME)_CPPFLAGS))		\
	--cppopts='-include "$(CONFIG_H)"' \
	--precomp=$($(NAME)_PRECOMP) $($(NAME)_HEADER))

.chs.pp.chs: $(CONFIG_H)
	@echo Preprocessing for $(NAME)
	$(strip $(HSCPP) $(AM_CPPFLAGS) \
	$(if $(NAME),$($(NAME)_CPPFLAGS) $($(NAME)_CFLAGS),$(CPPFLAGS)) \
	$(addprefix -include ,$(CONFIG_H)) \
	$< -o $@)

.hsc.hs: $(CONFIG_H)
	$(strip $(HSC2HS) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
        $(addprefix -L-optl,\
	$(AM_LDFLAGS) $($(NAME)_EXTRA_LIBS) $($(NAME)_LIBS)) \
        $(addprefix -C,	$(filter-out -I%,$(AM_CPPFLAGS)) \
	$($(NAME)_CFLAGS))\
        $(filter -I%,$(AM_CPPFLAGS)) \
	$($(NAME)_CPPFLAGS)\
	--include $(CONFIG_H) --include $($(NAME)_HEADER) \
        --cc=$(HC) --lflag=-no-hs-main $<)

.chs.hs: 
	@echo Building .hs file for $(NAME)
	$(if $(subst no,,$(BUILT_IN_C2HS)),$(strip \
	if test -x $(C2HS); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="tools_c2hs_c2hsLocal" \
	  tools/c2hs/c2hsLocal; fi;))
	$(strip if test -f $($(NAME)_PRECOMP); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) NAME="$(NAME)" $($(NAME)_PRECOMP); fi;)
	$(strip $(C2HS) $(C2HS_FLAGS) \
	+RTS $(HSTOOLFLAGS) -RTS \
	-i$(call pkgVPATH,$(NAME)) --precomp=$($(NAME)_PRECOMP) -o $@ $<)


# installation of packages

getVar			= $($(subst .,_,$(subst /,_,$(1)))_$(2))

install-data-hook :
	$(if $(PKGCONF),if test -f $(PKGCONF); then :; \
	else echo "[]" > $(PKGCONF); fi;)
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(GHCPKG) $(addprefix -f ,$(PKGCONF)) -u -g \
	-Dprefix=$(prefix) -Dexec_prefix=$(exec_prefix) -Dpkglibdir=$(pkglibdir)\
	-i $(call getVar,$(pkgname),PACKAGE);)

uninstall-hook :
	$(foreach pkgname,$(lib_LIBRARIES), \
	  $(GHCPKG) $(addprefix -f ,$(PKGCONF)) \
	  -r `cat $(call getVar,$(pkgname),PACKAGE) | $(GREP) name | $(SED) "s/ *name *= *\"\([a-zA-Z0-9]*\)\",/\1/"`;) \
	$(if $(PKGCONF),if test -f $(PKGCONF); then \
	  if test -n `head $(PKGCONF) | $(GREP) -e "\[\]"`; then \
	  $(RM) $(PKGCONF) $(PKGCONF).old; fi; \
	fi)

