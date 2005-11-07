# --*Makefile*--

EMPTY :=
SPACE := $(EMPTY) $(EMPTY)

# Cunning make hackery, this function translates from a file name to the
# package variable name prefix, eg
#   "gtk/Graphics/UI/Gtk.hs" to "libHSgtk_a"
# using a make var
#   gtk_PKGNAME = libHSgtk_a
#
PKG = \
  $(if $(call $(firstword $(subst /, ,$@))_PKGNAME,$@),$(strip \
       $(call $(firstword $(subst /, ,$@))_PKGNAME,$@)), \
     $(error PKG: cannot find PKGNAME for "$@", $(1) flag requested))

# necessary so support packages under the tools directory
tools_PKGNAME = $(call tools_$(word 2,$(subst /, ,$(1)))_PKGNAME,$(1))

pkgVPATH = $(subst $(SPACE),:,$($(PKG)_SOURCESDIRS))
getVar   = $($(subst .,_,$(subst /,_,$(1)))_$(2))

LINK = 	$(strip $(HC) -o $@ $(HCFLAGS) $($(PKG)_HCFLAGS) \
	$(addprefix -package ,$($(PKG)_PACKAGEDEPS)) \
	$(addprefix -optl,$(AM_LDFLAGS) $(LDFLAGS) $($(PKG)_LDFLAGS)))

#Using pattern rule here to prevent automake from understanding the rule
#and falsely concluding that two source files will produce the same object
#file even though the object files will be in different directories.
#Obviously the 'subdir-objects' option only works for C/C++ files.
if ENABLE_SPLITOBJS
%.o : %.hs $(CONFIG_HEADER)
	mkdir -p $*_split
	rm -f $*_split/*.o
	$(strip $(HC) +RTS $(HSTOOLFLAGS) -RTS \
	-c $< -o $@ $(HCFLAGS) $($(PKG)_HCFLAGS) \
	$(call getVar,$<,HCFLAGS) -i$(pkgVPATH) \
	$(addprefix -package-name ,$(notdir $(basename $(basename $($(PKG)_PACKAGE))))) \
	$(addprefix '-#include<,$(addsuffix >', $($(PKG)_HEADER))) \
	$(AM_CPPFLAGS) $($(PKG)_CPPFLAGS))
	touch $@
else
%.o : %.hs $(CONFIG_HEADER)
	$(strip $(HC) +RTS $(HSTOOLFLAGS) -RTS \
	-c $< -o $@ $(HCFLAGS) $($(PKG)_HCFLAGS) \
	$(call getVar,$<,HCFLAGS) -i$(pkgVPATH) \
	$(addprefix -package-name ,$(notdir $(basename $(basename $($(PKG)_PACKAGE))))) \
	$(addprefix '-#include<,$(addsuffix >', $($(PKG)_HEADER))) \
	$(AM_CPPFLAGS) $($(PKG)_CPPFLAGS))
endif

.DELETE_ON_ERROR : %.deps

# A string that is non-empty if dependencies should not be calculated. 
# All but the "clean" target are run during dependencies calculation
# and hence are listed here to avoid nasty recursion.
noDeps   := $(strip $(findstring clean,$(MAKECMDGOALS)) \
		    $(findstring c2hsLocal,$(MAKECMDGOALS)) \
		    $(findstring .hs,$(MAKECMDGOALS)) \
		    $(findstring .precomp,$(MAKECMDGOALS)))

# Dependencies are only calculated if the .deps files does not exist. 
# Thereafter it is never updated. A fix that likely works is to
# recalculate the dependencies of a .hs file each time it is
# recompiled. This does not work if some module reexports entities of
# another module. A sound fix would be to calculate dependencies each
# time which is too time consuming.

%.deps :
	$(if $(strip \
	  $(if $(findstring c2hs,$@),\
	  $(findstring clean,$(MAKECMDGOALS)),$(noDeps))),,\
	$(strip if test -f $@; then touch $@; else \
	touch $@; \
	$(if $(word 2,$($(PKG)_HSFILES)),\
	  $(MAKE) $(AM_MAKEFLAGS) $($(PKG)_HSFILES); \
	  $(HC) -M $(addprefix -optdep,-f $@) -fglasgow-exts \
	  $(HCFLAGS) $($(PKG)_HCFLAGS) -i$(pkgVPATH) \
	  $(AM_CPPFLAGS) $($(PKG)_CPPFLAGS) $($(PKG)_HSFILES);) \
	fi;))

.chs.dep :
	$(CHSDEPEND) -i$(pkgVPATH) $<

.hs.chi :
	@:

.o.hi:
	@:

.c.o:
	$(strip $(HC) -c $< -o $@ $(INCLUDES) \
		$(AM_CPPFLAGS) $($(PKG)_CPPFLAGS) $(CPPFLAGS) \
		$(addprefix -optc,$(AM_CFLAGS) $($(PKG)_CFLAGS) \
				  $(call getVar,$<,CFLAGS) $(CFLAGS)))

# The cheeky rule for .hi files says that .hi files can be created as
# side-effect of generating a .o file. Make sure the .hi files are not
# deleted as normal intermediate files are.
.PRECIOUS: %.hi

# Same for .chi
.PRECIOUS: %.chi

%.precomp :
	$(strip $(C2HS) $(C2HS_FLAGS)		\
	+RTS $(HSTOOLFLAGS) $(PROFFLAGS) -RTS		\
	$(addprefix -C,$($(PKG)_CFLAGS) $($(PKG)_CPPFLAGS))		\
	--cppopts='-include "$(CONFIG_HEADER)"' \
	--precomp=$($(PKG)_PRECOMP) $($(PKG)_HEADER))

.chs.pp.chs: $(CONFIG_HEADER)
	$(strip $(HSCPP) $(AM_CPPFLAGS) \
	$($(PKG)_CPPFLAGS) $($(PKG)_CFLAGS) \
	$(addprefix -include ,$(CONFIG_HEADER)) \
	$< -o $@)

.hsc.hs: $(CONFIG_HEADER)
	$(strip $(HSC2HS) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
        $(addprefix -L-optl,$(AM_LDFLAGS) $(LDFLAGS) $($(PKG)_LIBS)) \
        $(addprefix -C,	$(filter-out -I%,$(AM_CPPFLAGS) $(CPPFLAGS)) \
	$(addprefix -optc,$(AM_CFLAGS) $(CFLAGS) $($(PKG)_CFLAGS)))\
        $(filter -I%,$(AM_CPPFLAGS) $(CPPFLAGS)) $($(PKG)_CPPFLAGS)\
	-C'-optc-include' -C'-optc$(CONFIG_HEADER)' \
	--include $($(PKG)_HEADER) \
        --cc=$(HC) --lflag=-no-hs-main $<)

.chs.hs: 
	$(if $(subst no,,$(BUILT_IN_C2HS)),$(strip \
	if test -x $(C2HS); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) \
	  tools/c2hs/c2hsLocal$(EXEEXT); fi;))
	$(strip if test -f $($(PKG)_PRECOMP); then :; else \
	  $(MAKE) $(AM_MAKEFLAGS) $($(PKG)_PRECOMP); fi;)
	$(strip $(C2HS) $(C2HS_FLAGS) \
	+RTS $(HSTOOLFLAGS) -RTS \
	-i$(pkgVPATH) --precomp=$($(PKG)_PRECOMP) -o $@ $<)

