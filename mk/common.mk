# --*Makefile*--

# regular expressions to eliminate ../ and ./ in paths
SEDNOUP		:= 's+\(.*\)/.*/\.\./\(.*\)+\1/\2+'
SEDNOID		:= 's+\(.*\)\./\(.*\)+\1\2+'
SEDPIPE		:= $(SED) -e $(SEDNOUP) -e $(SEDNOID) -e $(PATHSED)

# directories of interest
ABS_TOP		:= $(shell echo $(ABS_TOP) | $(SEDPIPE))
CURDIR		:= $(shell $(PWD) | $(SEDPIPE))
TARDIR		:= $(subst $(ABS_TOP),$(TARNAME),$(CURDIR))/

# directories of source files
SUBDIRS			?= .

SUBDIRSOK		:= $(dir $(addsuffix /,$(SUBDIRS)))

# The user supplied subdirectory where the installed files should go.
INSTALLDIROK		= $(strip $(if $(INSTALLDIR),\
			/$(patsubst %/,%,$(dir $(INSTALLDIR)/))))

# directories of installation
INST_LIBDIR		= $(shell echo $(addsuffix $(INSTALLDIROK),$(libdir)) | $(SEDPIPE))
INST_HIDIR		= $(INST_LIBDIR)/hi
INST_INCLDIR		= $(INST_HIDIR)
INST_BINDIR		= $(shell echo $(addsuffix $(INSTALLDIROK),$(bindir)) | $(SEDPIPE))
INST_DOCDIR		= $(datadir)/doc/gtk2hs

# these values are used for building a library in-place
INPL_HIDIR		:= $(sort $(patsubst %/.,%,$(patsubst %/,%,\
			   $(dir $(addprefix $(CURDIR)/,$(SUBDIRSOK))))))
INPL_LIBDIR		:= $(patsubst %/,%,$(CURDIR))
INPL_INCLDIR		:= $(INPL_HIDIR)



# CHSFILES = EXPLICIT_HEADER \dotcup STANDARD_HEADER, in the right order
# EXTRA_CHSFILES contains generated .chs files. It is important that the
# sequence reflects the dependencies of the files because this information
# is extracted from the files themselves (and generated files don't exist
# in a clean tree).
CHSFILES 		:= $(filter-out $(EXTRA_CHSFILES),\
			$(patsubst ./%,%,\
			$(foreach DIR,$(SUBDIRSOK),$(wildcard $(DIR)*.chs))))

ALLCHSFILES		:= $(NEEDCHI:=.chs) $(filter-out $(NEEDCHI:=.chs), \
			$(CHSFILES) $(EXTRA_CHSFILES))

# all .chs files that have a .chs-HEADER variable defined
EXPLICIT_HEADER         := $(foreach FILE,$(ALLCHSFILES),\
                        $(if $(findstring undefined,\
                        $(origin $(notdir $(basename $(FILE)))-HEADER)),,\
			$(FILE)))

# all .chs files that use the common header file in HEADER
STANDARD_HEADER         := $(filter-out $(EXPLICIT_HEADER),$(ALLCHSFILES))

# HSC files
HSCFILES                := $(filter-out $(EXTRA_HSCFILES),\
			$(patsubst ./%,%,\
                        $(foreach DIR,$(SUBDIRSOK),$(wildcard $(DIR)*.hsc))))\
                        $(EXTRA_HSCFILES)

# These are all .hs files that are not generated in any way.
HSFILES  		:= $(filter-out $(ALLCHSFILES:.chs=.hs)\
			$(HSCFILES:.hsc=.hs) $(EXTRA_HSFILES),\
			$(patsubst ./%,%,\
			$(foreach DIR,$(SUBDIRSOK),$(wildcard $(DIR)*.hs))))

# These are all .hs files in the project. This is not the same as *.hs in
# all subdirs because in a clean tree there is e.g. no .hs for a .chs file.
ALLHSFILES		:= $(HSFILES) $(ALLCHSFILES:.chs=.hs) \
			$(HSCFILES:.hsc=.hs) $(EXTRA_HSFILES)

# Possibly useful: These are the files that are hand-crafted:
ALLSOURCEFILES		:= $(ALLCHSFILES) $(HSCFILES) $(HSFILES) \
			$(EXTRA_HSFILES)

EXTRA_HFILESOK		:= $(sort $(EXTRA_HFILES) $(EXTRA_CFILES:.c=.h))

# C include file paths and other options to CPP.
EXTRA_CPPFLAGS_ONLY_I	:= $(filter -I%,$(EXTRA_CPPFLAGS))

EXTRA_LIBS_ONLY_Ll	:= $(filter -L% -l%,$(EXTRA_LIBS))

EXTRA_LIBS_ONLY_L	:= $(filter -L%,$(EXTRA_LIBS_ONLY_Ll))

CPPFLAGS_ONLY_I		:= $(filter -I%,$(CPPFLAGS))

LIBS_ONLY_L		:= $(filter -L% -l%,$(CFLAGS))


# Ensure that the user-supplied target is valid. If there is a nice way to
# capitalize the first letter, do that to $(APPNAME).hs and add that here.
MAIN			?= Main.hs $(APPNAME).hs

MAINOK			:= $(filter $(MAIN) $(addsuffix $(MAIN),$(SUBDIRSOK)),\
			$(patsubst ./%,%,$(ALLHSFILES)))

EMPTY			:=

COMMA			:= ,

SPACE			:= $(EMPTY) $(EMPTY)

# Turn a space separated directory list into a colon separated one.
HIDIRSOK		:= $(subst $(SPACE),:,$(strip \
			$(patsubst %/,%,$(HIDIRS) $(SUBDIRSOK))))

NEEDPACKAGESOK		:= $(addprefix -package ,$(strip $(NEEDPACKAGES)))

HCINCLUDES		:= $(addprefix '-\#include<,$(addsuffix >',$(HEADER) \
			$(EXTRA_HFILESOK)))


# Specify how hsc should be run.
HSCFLAGGED	:= $(strip $(HSC) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
		$(EXTRA_CPPFLAGS_ONLY_I) $(CPPFLAGS_ONLY_I) \
		$(addprefix --lflag=,$(EXTRA_LIBS_ONLY_Ll) $(LIBS_ONLY_L)\
		$(addprefix --cflag=,$(CPPFLAGS) $(EXTRA_CPP_FLAGS)))\
		--cc=$(HC) --lflag=-no-hs-main)

# Specify how c2hs should be run.
C2HSFLAGGED	:= $(C2HS) $(C2HSFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
		$(addprefix -C,$(EXTRA_CPPFLAGS_ONLY_I) $(CPPFLAGS_ONLY_I)) \
		-i$(HIDIRSOK) $(C2HS_EXTRA_FLAGS)

# Read in all extra dependencies between .chs files.
-include $(ALLCHSFILES:.chs=.dep)

# Quick and dirty dependency to force the compilation of .chs file if
# a current version of the .chi file is needed.
%.chi : %.hs

define runC2HS
@if test -f .depend; then \
  echo "$(C2HSFLAGGED) -o : $(HEADER)" `cat .depend` && \
  ($(C2HSFLAGGED) -o : $(HEADER) `cat .depend` || \
  (echo removing `cat .depend | "$(SED) s/\(.*\)\.chs/\1.hs/"`; \
  $(RM) `cat .depend | $(SED) "s/\(.*\)\.chs/\1.hs/"` .depend; \
  exit 1)) && \
  echo "$(TOP)/mk/chsDepend -i$(HIDIRSOK)" `cat .depend` && \
  $(TOP)/mk/chsDepend -i$(HIDIRSOK) `cat .depend` && \
  $(RM) .depend; \
fi
endef

# How to build <blah.hs> from <blah.chs>: Since <blah.chs-HEADER> is defined
# we will use the specified header file. We invoke c2hs for each .chs file
# anew.
$(EXPLICIT_HEADER:.chs=.hs) : %.hs : %.chs
	$(runC2HS)
	$(strip $(C2HSFLAGGED) -o $(addsuffix .hs,$(basename $<)) \
	  $($(addsuffix -HEADER,$(notdir $(basename $@)))) $<)
	$(TOP)/mk/chsDepend -i$(HIDIRSOK) $<

# As above, but <blah.chs-HEADER> is not defined so we use the variable
# HEADER which contains the name of the header file common to all
# files in STANDARD_HEADER. This is a major performance improvment as
# c2hs has to parse the header file only once in order to translate
# several .chs files.
# The actual rebuilt of the files is delayed until either c2hs needs the
# .chi or ghc needs the .hs files. Until then, all files that need to be
# rebuilt are stored in .depend . This is a kind of hack; make is good in
# breaking down larger dependencies into smaller one, but the other way round
# seems to be impossible: The variable @? does indeed contain all the
# targets that need to be updated, but updating these files does not convince
# make not to rerun the rule for another file in @?.
$(STANDARD_HEADER:.chs=.hs) : %.hs : %.chs
	echo $< >> .depend
	touch $@

# How to build <blah.hs> from <blah.hsc>
$(HSCFILES:.hsc=.hs) : %.hs : %.hsc
	$(HSCFLAGGED) $<


# Goals for applications and libraries.

.PHONY: errorNoTarget noTarget

errorNoTarget	:
		@echo You need to set PACKAGENAME to build a library or
		@echo APPNAME to build an executable. 


inplaceinit	:
	@if test ! -f $(LOCALPKGCONF); then \
	  echo [ Package {\
	  name = \"defaultPackage\",\
	  import_dirs     = [],\
	  source_dirs     = [],\
	  library_dirs    = [],\
	  hs_libraries    = [],\
	  extra_libraries = [],\
	  include_dirs    = [],\
	  c_includes      = [],\
	  package_deps    = [],\
	  extra_ghc_opts  = [],\
	  extra_cc_opts   = [],\
	  extra_ld_opts   = []}] > $(LOCALPKGCONF); \
	  echo Generating new local package file \"$(LOCALPKGCONF)\".; \
	fi

ifneq ($(strip $(PACKAGENAME)),)

include $(TOP)/mk/library.mk

inplace	: $(TARGETOK)

all	: inplace

else
ifneq ($(strip $(APPNAME)),)

include $(TOP)/mk/application.mk

all	: $(TARGETOK)

else

TARGETOK		= errorNoTarget

endif
endif


targets :
	@echo all	in subdirs: builds libraries/applications
	@echo		in toplevel: equivalent to "make all" in all subdirs
	@echo inplace	"all" in all subdirs and entry to local package file
	@echo noinplace removes the entry from the local package file
	@echo install	"all", installs files, add entry to GHC's package file
	@echo uninstall reverts install and entry-adding effects of "install"
	@echo mostlyclean remove all object, libraries and application files
	@echo clean     "mostlyclean" and remove all generated .hs files
	@echo distclean "clean" and remove generated .chs files

.PHONY: debug 
debug 	:
	@echo top: $(TOP)
	@echo SED: $(SEDPIPE)
	@echo cur dir: $(CURDIR)
	@echo tar into: $(TARDIR)
	@echo install into: $(INSTALLDIROK)
#	@echo Goal: $(MAINOK)
#	@echo Target: $(TARGETOK)
#	@echo Library: $(LIBNAME)
#	@echo Application: $(APPNAME)
	@echo EXTRA_CPPFLAGS: $(EXTRA_CPPFLAGS_ONLY_I)
#	@echo all CHS files: $(CHSFILES)
	@echo Standard header: $(STANDARD_HEADER)
	@echo Explicit header: $(EXPLICIT_HEADER)
#	@echo all HSC files: $(HSCFILES)
#	@echo all other HS files: $(HSFILES)
	@echo stub-o files: $(filter-out %*_stub.o,\
	  $(shell echo $(addsuffix *_stub.o,$(SUBDIRSOK))))
	@echo hi: $(INST_HIDIR) lib: $(INST_LIBDIR) 
	@echo incl: $(INST_INCLDIR) bin: $(INST_BINDIR)
#	@echo user install dir: $(INSTALLDIR)
	@echo subdirs: $(SUBDIRSOK)
	@echo $(ALLSOURCEFILES) > sourcefiles.txt
#	@cvs status $(CLEANFILES) 2> /dev/null | $(GREP) File | $(GREP) Unknown

# Create a source tar achive. Do this by adding files to the tar file in the
# top-level directory.
tarsource :
	$(strip $(TAR) rf $(TOP)/$(TARNAME).tar -C $(TOP)\
	  $(foreach FILE,Makefile $(CHSFILES)\
	    $(filter-out $(EXTRA_HSCFILES), $(HSCFILES))\
	    $(filter-out $(EXTRA_HSFILES), $(HSFILES))\
	    $(EXTRA_CFILES) $(EXTRA_HFILESOK) $(EXTRA_TARFILES),\
	    $(shell echo $(TARDIR)$(FILE) | $(SEDPIPE))))
	  


.PHONY: install installdirs installcheck uninstall


# from `info standards':
#
#`clean'
#     Delete all files from the current directory that are normally
#     created by building the program.  Don't delete the files that
#     record the configuration.  Also preserve files that could be made
#     by building, but normally aren't because the distribution comes
#     with them.
#
#     Delete `.dvi' files here if they are not part of the distribution.
#
#`distclean'
#     Delete all files from the current directory that are created by
#     configuring or building the program.  If you have unpacked the
#     source and built the program without creating any other files,
#     `make distclean' should leave only the files that were in the
#     distribution.
#
#`mostlyclean'
#     Like `clean', but may refrain from deleting a few files that people
#     normally don't want to recompile.  For example, the `mostlyclean'
#     target for GCC does not delete `libgcc.a', because recompiling it
#     is rarely necessary and takes a lot of time.
#
#`maintainer-clean'
#     Delete almost everything from the current directory that can be
#     reconstructed with this Makefile.  This typically includes
#     everything deleted by `distclean', plus more: C source files
#     produced by Bison, tags tables, Info files, and so on.
#
#     The reason we say "almost everything" is that running the command
#     `make maintainer-clean' should not delete `configure' even if
#     `configure' can be remade using a rule in the Makefile.  More
#     generally, `make maintainer-clean' should not delete anything that
#     needs to exist in order to run `configure' and then begin to build
#     the program.  This is the only exception; `maintainer-clean' should
#     delete everything else that can be rebuilt.
#
#      The `maintainer-clean' target is intended to be used by a
#     maintainer of the package, not by ordinary users.  You may need
#     special tools to reconstruct some of the files that `make
#     maintainer-clean' deletes.  Since these files are normally
#     included in the distribution, we don't take care to make them easy
#     to reconstruct.  If you find you need to unpack the full
#     distribution again, don't blame us.


.PHONY: clean distclean mostlyclean maintainer-clean

mostlyclean : noinplace
	$(strip $(RM) $(TARGETOK) $(ALLHSFILES:.hs=.o) $(ALLHSFILES:.hs=.hi) \
	  $(EXTRA_CFILES:.c=.o) $(ALLHSFILES:.hs=_stub.*) .depend \
	  $(ALLCHSFILES:.chs=.dep))

clean	: mostlyclean
	$(strip $(RM) $(ALLCHSFILES:.chs=.hs) $(ALLCHSFILES:.chs=.chi) \
	  $(HSCFILES:.hsc=.hs) $(EXTRA_CLEANFILES))

distclean : clean
	$(strip $(RM) $(EXTRA_HSFILES) $(EXTRA_CHSFILES) \
	  $(ALLCHSFILES:.chs=.dep) $(LOCALPKGCONF) $(LOCALPKGCONF).old \
	  $(EXTRA_DISTCLEANFILES))

maintainer-clean : distclean


