# This file describes what to do in order to build a package for GHC.

LIBNAME			?= $(PACKAGENAME)

TARGETOK		= $(addprefix $(strip $(LIBPREFIX)),\
			$(addsuffix $(LIBSUFFIX),$(strip $(LIBNAME))))


makeTextList		= $(addprefix \",$(addsuffix \",\
			$(subst $(SPACE),\"$(COMMA)\",$(sort $(1)))))

noinplace : inplaceinit
	@if $(PKG) -f $(LOCALPKGCONF) -l | $(GREP) $(PACKAGENAME) \
	  > /dev/null; then \
	  echo Removing old local entry for \"$(PACKAGENAME)\".; \
          $(PKG) -f $(LOCALPKGCONF) -r $(PACKAGENAME) > /dev/null; \
	fi

inplace	: noinplace
	@echo Adding package description to local package file.
	@echo Package {\
	  name			= \"$(PACKAGENAME)\",\
	  import_dirs		= [$(call makeTextList, $(INPL_HIDIR))],\
	  source_dirs		= [],\
	  library_dirs		= [$(call makeTextList, $(INPL_LIBDIR) \
	  $(patsubst -L%,%,$(filter -L%,$(EXTRA_LIBS_ONLY_L) \
	  $(LIBS_ONLY_L))))],\
	  hs_libraries		= [\"$(LIBNAME)\"],\
	  extra_libraries	= [$(call makeTextList,\
	  $(patsubst -l%,%,$(filter -l%,$(EXTRA_LIBS_ONLY_Ll) \
	  $(LIBS_ONLY_L))))],\
	  include_dirs		= [$(call makeTextList, $(INPL_INCLDIR)\
	  $(patsubst -I%,%,$(EXTRA_CPPFLAGS_ONLY_I)))],\
	  c_includes		= [$(call makeTextList,\
	  			  $(notdir $(STUBHFILES)) $(HEADER)\
				  $(EXTRA_HFILESOK))],\
	  package_deps		= [$(call makeTextList,$(NEEDPACKAGES))],\
	  extra_ghc_opts	= [$(call makeTextList,$(EXTRAHC_FLAGS))],\
	  extra_cc_opts		= [],\
	  extra_ld_opts		= [$(call makeTextList,\
          $(addprefix -Wl$(COMMA),\
	  $(addprefix --subsystem$(SPACE),$(SUBSYSTEM))) \
	  $(addprefix -u ,$(EXTRA_SYMBOLS)))]} | \
	  $(PKG) --force -f $(LOCALPKGCONF) -a  > /dev/null

installcheck :
	@if $(PKG) -l | $(GREP) $(PACKAGENAME) > /dev/null; then \
	  echo There is already a global package of the name $(PACKAGENAME).; \
	  echo Remove this package with \`make uninstall\' first.; \
	  exit 1; \
	fi

installdirs :
	$(INSTALL) -d $(INST_HIDIR) $(INST_LIBDIR) $(INST_INCLDIR)

install : installcheck $(TARGETOK) installdirs installfiles \
	  installpackage #interactiveInstall

installfiles :
	$(INSTALL) -m644 $(ALLHSFILES:.hs=.hi) $(INST_HIDIR)
	$(INSTALL) -m644 $(TARGETOK) $(INST_LIBDIR)
	$(TOUCH) -r $(TARGETOK) $(INST_LIBDIR)/$(TARGETOK)
ifneq ($(strip $(STUBHFILES) $(EXTRA_HFILESOK)),)
	$(INSTALL) -m644 $(STUBHFILES) $(EXTRA_HFILESOK) $(INST_INCLDIR)
endif

installpackage :
	@echo Package {\
	  name			= \"$(PACKAGENAME)\",\
	  import_dirs		= [\"$(INST_HIDIR)\"],\
	  source_dirs		= [],\
	  library_dirs		= [$(call makeTextList, $(INST_LIBDIR) \
	  $(patsubst -L%,%,$(EXTRA_LIBS_ONLY_L)))],\
	  hs_libraries		= [\"$(LIBNAME)\"],\
	  extra_libraries	= [$(call makeTextList,\
	  $(patsubst -l%,%,$(filter -l%,$(EXTRA_LIBS_ONLY_Ll) \
	  $(LIBS_ONLY_L))))],\
	  include_dirs		= [$(call makeTextList, $(INST_INCLDIR)\
	  $(patsubst -I%,%,$(EXTRA_CPPFLAGS_ONLY_I)))],\
	  c_includes		= [$(call makeTextList,\
	  			  $(notdir $(STUBHFILES)) $(HEADER)\
				  $(notdir $(EXTRA_HFILESOK)))],\
	  package_deps		= [$(call makeTextList,$(NEEDPACKAGES))],\
	  extra_ghc_opts	= [$(call makeTextList,$(EXTRAHC_FLAGS))],\
	  extra_cc_opts		= [],\
	  extra_ld_opts		= [$(call makeTextList,\
          $(addprefix -Wl$(COMMA),\
	  $(addprefix --subsystem$(SPACE),$(SUBSYSTEM))) \
	  $(addprefix -u ,$(EXTRA_SYMBOLS)))]} | \
	  $(PKG) --force -a

uninstall : uninstallfiles uninstallpackage

uninstallfiles :
	$(RM) $(addprefix $(INST_INCLDIR)/,$(notdir $(ALLHSFILES:.hs=.hi)))
	$(RM) $(addprefix $(INST_LIBDIR)/,$(TARGETOK))
	$(RM) $(addprefix $(INST_INCLDIR)/,$(notdir $(STUBHFILES)\
	  ) $(EXTRA_CFILES:.c=.h))
	$(strip rmdir -p $(sort $(INST_HIDIR) $(INST_LIBDIR) \
	  $(INST_INCLDIR)) 2> /dev/null || true)

#	  echo $(TOP)/mk/chsDepend -i$(HIDIRSOK) `cat .depend`\
#	  echo $(C2HSFLAGGED) -o : $(HEADER) `cat .depend`\

uninstallpackage :
	$(PKG) -r $(PACKAGENAME)

$(TARGETOK) : $(ALLHSFILES) $(EXTRA_CFILES:.c=$(OBJSUFFIX)) $(GHCILIBS:\
	      $(LIBSUFFIX)=$(OBJSUFFIX)) $(GHCIOBJS)
	@if test -f .depend; then \
	  echo "$(C2HSFLAGGED) -o : $(HEADER)" `cat .depend` &&\
	  $(C2HSFLAGGED) -o : $(HEADER) `cat .depend` && \
	  echo "$(TOP)/mk/chsDepend -i$(HIDIRSOK)" `cat .depend` &&\
	  $(TOP)/mk/chsDepend -i$(HIDIRSOK) `cat .depend` && \
	  $(RM) .depend;\
	fi
	$(RM) $@
	$(strip $(HC) --make $(MAINOK) -package-name $(PACKAGENAME) \
	  -package-conf $(LOCALPKGCONF) $(HCINCLUDES) \
	  $(EXTRA_CPPFLAGS_ONLY_I) $(EXTRA_LIBS_ONLY_Ll) \
	  $(LIBS_ONLY_L) $(CPPFLAGS_ONLY_I) \
	  $(HC_FLAGS) $(EXTRAHC_FLAGS) -i$(HIDIRSOK) $(NEEDPACKAGESOK))
	$(strip $(AR) crs $@ $(STUBOFILES) $(ALLHSFILES:.hs=$(OBJSUFFIX)) \
	  $(EXTRA_CFILES:.c=$(OBJSUFFIX)))

# $(STUBOFILES) $(addprefix -#include ,$(STUBHFILES))

# GHCi handling
# The current version of GHCi cannot load lib<blah>.a files. We have to convert
# them into object files <blah>.o and make it accessible to GHCi. The decision
# here is to convert the library and header files (.hi) into <blah>.o and copy
# them into the installation directory $(INST_LIBDIR).



GHCIARCH		= $(wildcard $(addprefix lib,$(addsuffix $(LIBSUFFIX),\
			  $(strip $(LIBNAME)))) $(foreach DIR,$(patsubst \
			  -L%,%,$(EXTRA_LIBS_ONLY_L)), $(foreach FILE,\
			  $(addprefix $(LIBPREFIX), $(addsuffix $(LIBSUFFIX),\
			  $(strip $(patsubst -l%,%,$(filter -l%, \
			  $(EXTRA_LIBS)))))),$(DIR)/$(FILE))))

GHCIOBJS		= $(addprefix $(INST_LIBDIR)/,$(patsubst \
			  lib%$(LIBSUFFIX),%$(OBJSUFFIX),\
			  $(notdir $(GHCIARCH))))

%convert		:
	$(LD) -r --whole-archive -o $(patsubst !%,%,$(filter !%,$(subst !,\
	  !$(SPACE)!,$(patsubst %convert,%,$@)))) $(patsubst %!,%,$(filter \
	  %!,$(subst !,!$(SPACE)!,$(patsubst %convert,%,$@))))
	chmod 644 $(patsubst !%,%,$(filter !%,$(subst !,!$(SPACE)!,\
	  $(patsubst %convert,%,$@))))

#interactiveInstall 	: $(join $(addsuffix !,$(GHCIARCH)),\
#			  $(addsuffix convert,$(GHCIOBJS)))

interactiveUninstall :
	$(RM) $(GHCIOBJS)

idbg	:
	@echo $(GHCIARCH)
