# --*Makefile*--

# directories of source files

SUBDIRS			?= .

SUBDIRSOK		= $(dir $(addsuffix /,$(SUBDIRS)))

# The user supplied subdirectory where the installed files should go.
INSTALLDIROK		= $(strip $(if $(INSTALLDIR),\
			/$(patsubst %/,%,$(dir $(INSTALLDIR)/))))

# directories of installation
INST_HIDIR		?= $(addsuffix $(INSTALLDIROK)/hi,$(libdir))
INST_LIBDIR		?= $(addsuffix $(INSTALLDIROK),$(libdir))
INST_INCLDIR		?= $(INST_HIDIR)
INST_BINDIR		?= $(addsuffix $(INSTALLDIROK),$(bindir))

# these values are used for building a library in-place
INPL_HIDIR		= $(sort $(patsubst %/.,%,$(patsubst %/,%,\
			  $(dir $(addprefix $(CURDIR)/,$(SUBDIRSOK))))))
INPL_LIBDIR		= $(patsubst %/,%,$(CURDIR))
INPL_INCLDIR		= $(INPL_HIDIR)


# CHSFILES = EXPLICIT_HEADER \dotcup STANDARD_HEADER
CHSFILES 		= $(filter-out $(EXTRA_CHSFILES),\
			$(foreach DIR,$(SUBDIRSOK),$(wildcard $(DIR)*.chs)))\
			$(EXTRA_CHSFILES)

# These are the files without the prerequisite CHS files.
CHSFILESAUTO		= $(filter-out $(NEEDCHS), $(CHSFILES))

HSCFILES		= $(filter-out $(EXTRA_HSCFILES),\
			$(foreach DIR,$(SUBDIRSOK),$(wildcard $(DIR)*.hsc)))\
			$(EXTRA_HSCFILES)

# all .chs files that have a .chs-HEADER variable defined
EXPLICIT_HEADER 	= $(foreach FILE,$(CHSFILESAUTO),\
			$(if $(findstring undefined,\
			$(origin $(notdir $(basename $(FILE)))-HEADER)),,$(FILE)))

# all .chs files that use the common header file in HEADER
STANDARD_HEADER 	= $(filter-out $(EXPLICIT_HEADER),$(CHSFILESAUTO))

# We cannot generate dependencies between .chs and .chi files. We therefore
# compile explicitly named .chs files first (by means of evaluating NEEDCHI).
NEEDCHS			= $(addsuffix .chs,$(NEEDCHI))

# This is EXPLICIT_HEADER for the files in NEEDCHS
NEEDCHS_EXPLICIT      	= $(foreach FILE,$(NEEDCHS),\
			$(if $(findstring undefined,\
			$(origin $(notdir $(basename $(FILE)))-HEADER)),,\
			$(FILE)))

# STANDARD_HEADER for NEEDCHS
NEEDCHS_STANDARD	= $(filter-out $(NEEDCHS_EXPLICIT),$(NEEDCHS))

# These are all .hs files that are not generated in any way.
HSFILES  		= $(filter-out $(CHSFILES:.chs=.hs)\
			$(NEEDCHS:.chs=.hs) \
			$(HSCFILES:.hsc=.hs) $(EXTRA_HSFILES),\
			$(foreach DIR,$(SUBDIRSOK),$(wildcard $(DIR)*.hs)))

# These are all .hs files in the project. This is not the same as *.hs in
# all subdirs because in a clean tree there is e.g. no .hs for a .chs file.
ALLHSFILES		= $(HSFILES) $(CHSFILES:.chs=.hs) \
			$(HSCFILES:.hsc=.hs) $(EXTRA_HSFILES)

# Possibly useful: These are the files that are hand-crafted:
ALLSOURCEFILES		= $(CHSFILES) $(NEEDCHS) $(HSCFILES) $(HSFILES)\
			$(EXTRA_HSFILES)

# Compile a list of all generated *_stub.o files. Such a file is generated if
# a sourcefile contains a foreign export declaration. If there is a standard
# grep for regexs, then we should match for the beginning of the line.
STUBOFILES		= $(strip \
	$(patsubst %.hs,%_stub.o, $(foreach FILE,\
	$(HSFILES),$(shell $(GREP) -l "foreign export" $(FILE)))) \
	$(patsubst %.chs,%_stub.o, $(foreach FILE,\
	$(filter-out $(EXTRA_CHSFILES),$(CHSFILES)),\
	$(shell $(GREP) -l "foreign export" $(FILE)))) \
	$(patsubst %.hsc,%_stub.o, $(foreach FILE,\
	$(HSCFILES),$(shell $(GREP) -l "foreign export" $(FILE))))\
	$(patsubst %.chs,%_stub.o, $(EXTRA_STUBFILES)))

# Not needed at the moment: We only include the header file in $(HEADER) and
# clean the tree through a wildcard.
STUBHFILES		= $(STUBOFILES:.o=.h)

EXTRA_HFILESOK		= $(sort $(EXTRA_HFILES) $(EXTRA_CFILES:.c=.h))

# C include file paths and other options to CPP.
EXTRA_CPPFLAGS_ONLY_I	= $(filter -I%,$(EXTRA_CPPFLAGS))

EXTRA_LIBS_ONLY_L	= $(filter -L%,$(EXTRA_LIBS))

CPPFLAGS_ONLY_I		= $(filter -I%,$(CPPFLAGS))

CFLAGS_ONLY_L		= $(filter -L%,$(CFLAGS))


# Ensure that the user-supplied target is valid. If there is a nice way to
# capitalize the first letter, do that to $(APPNAME).hs and add that here.
MAIN			?= Main.hs $(APPNAME).hs

MAINOK			= $(filter $(MAIN) $(addsuffix $(MAIN),$(SUBDIRSOK)),\
			  $(patsubst ./%,%,$(ALLHSFILES)))

EMPTY			=

COMMA			= ,

SPACE			= $(EMPTY) $(EMPTY)

# Turn a space separated directory list into a colon separated one.
HIDIRSOK		= $(subst $(SPACE),:,$(strip \
			  $(patsubst %/,%,$(HIDIRS) $(SUBDIRSOK))))

NEEDPACKAGESOK		= $(addprefix -package ,$(strip $(NEEDPACKAGES)))

HCINCLUDES		= $(addprefix '-\#include<,$(addsuffix >',$(HEADER) \
			  $(EXTRA_HFILESOK)))




# Specify how hsc should be run.
HSCFLAGGED	= $(strip $(HSC) $(HSCFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
		  $(EXTRA_CPPFLAGS_ONLY_I) \
		  $(addprefix --lflag=,$(EXTRA_LIBS) $(CFLAGS)\
		  $(addprefix --cflag=,$(CPPFLAGS) \
		  $(filter-out $(EXTRA_CPP_FLAGS_ONLY_I),$(EXTRA_CPP_FLAGS))))\
		  --cc=$(HC))

# Specify how c2hs should be run.
C2HSFLAGGED	= $(C2HS) $(C2HSFLAGS) +RTS $(HSTOOLFLAGS) -RTS \
		  $(addprefix -C,$(EXTRA_CPPFLAGS_ONLY_I)) -i$(HIDIRSOK)

# How to build <blah.hs> from <blah.hsc>
$(HSCFILES:.hsc=.hs) : %.hs : %.hsc
	$(HSCFLAGGED) $<

# How to build <blah.hs> from <blah.chs>: Since <blah.chs-HEADER> is defined
# we will use the specified header file. We invoke c2hs for each .chs file
# anew.
# The result is first dumped into .broken.hs files, then we fix a pointer
# problem that has been in c2hs for a while. The version of c2hs that can
# handle multiple files seems to be fixed, so no need for the repair.
$(EXPLICIT_HEADER:.chs=.hs) : %.hs : %.chs $(addsuffix .chi, $(NEEDCHI))
	$(strip $(C2HSFLAGGED) -o $(@:.hs=.broken.hs) \
	  $($(addsuffix -HEADER,$(notdir $(basename $@)))) $<)
	$(SED) "s/Ptr (FunPtr/(FunPtr/" $(@:.hs=.broken.hs) > $@
	mv $(@:.hs=.broken.chi) $(@:.hs=.chi)
	$(RM) $(@:.hs=.broken.hs)

# As above, but <blah.chs-HEADER> is not defined so we use the variable
# HEADER which contains the name of the header file common to all
# files in STANDARD_HEADER. This is a major performance improvment as
# c2hs has to parse the header file only once in order to translate
# several .chs files.
# About the "if" statement: If one of the needed .chs files has changed
# we need to rebuild the all standard header files (because we assume
# that they all depend on the needed files). If all needed .chs files
# stayed the same but some of the standard header files have changed,
# we will update only these.
$(STANDARD_HEADER:.chs=.hs) : $(STANDARD_HEADER) $(addsuffix .chi, $(NEEDCHI))
	$(strip $(C2HSFLAGGED) -o : $(HEADER) $(if $(filter %.chi,$?),\
          $(STANDARD_HEADER),$(filter %.chs,$?)))


# The same cases doubled for the NEEDCHS files.
$(NEEDCHS_EXPLICIT:.chs=.hs) : %.hs : %.chs
	$(strip $(C2HSFLAGGED) -o $(@:.hs=.broken.hs) \
	  $($(addsuffix -HEADER,$(notdir $(basename $@)))) $<)
	$(SED) "s/Ptr (FunPtr/(FunPtr/" $(@:.hs=.broken.hs) > $@
	mv $(@:.hs=.broken.chi) $(@:.hs=.chi)
	$(RM) $(@:.hs=.broken.hs)

$(NEEDCHS_STANDARD:.chs=.hs) : $(NEEDCHS_STANDARD)
	$(strip	$(C2HSFLAGGED) -o : $(HEADER) $?)

%.chi : %.hs ;



# Set up include file for either applications or libraries.

.PHONY: errorNoTarget noTarget

errorNoTarget	:
		@echo You need to set PACKAGENAME to build a library or
		@echo APPNAME to build an executable. 

inplace		: all

uninplace	:

all		: inplaceinit

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

else
ifneq ($(strip $(APPNAME)),)

include $(TOP)/mk/application.mk

else

TARGETOK		= errorNoTarget

endif
endif


all	: $(TARGETOK)

targets :
	@echo all	in subdirs: builds libraries/applications
	@echo		in toplevel: equivalent to "inplace" in all subdirs
	@echo inplace	"all" in all subdirs and entry to local package file
	@echo noinplace removes the entry from the local package file
	@echo install	"all", installs files, add entry to GHC's package file
	@echo uninstall reverts install and entry-adding effects of "install"
	@echo mostlyclean remove all object, libraries and application files
	@echo clean     "mostlyclean" and remove all generated .hs files
	@echo distclean "clean" and remove generated .chs files

.PHONY: debug 
debug 	:
	@echo Goal: $(MAINOK)
	@echo Target: $(TARGETOK)
	@echo Library: $(LIBNAME)
	@echo Application: $(APPNAME)
	@echo EXTRA_CPPFLAGS: $(EXTRA_CPPFLAGS_ONLY_I)
	@echo all CHS files: $(CHSFILES)
	@echo all HSC files: $(HSCFILES)
	@echo all other HS files: $(HSFILES)
	@echo all files generating stubs: $(STUBOFILES)
#	@echo hi: $(INST_HIDIR) lib: $(INST_LIBDIR) 
#	@echo incl: $(INST_INCLDIR) bin: $(INST_BINDIR)
#	@echo user install dir: $(INSTALLDIR)
	@echo subdirs: $(SUBDIRSOK)
#	@echo $(ALLSOURCEFILES) > sourcefiles.txt
#	@cvs status $(CLEANFILES) 2> /dev/null | $(GREP) File | $(GREP) Unknown

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

mostlyclean : uninplace
	$(strip $(RM) $(TARGETOK) $(ALLHSFILES:.hs=.o) $(ALLHSFILES:.hs=.hi) \
	  $(EXTRA_CFILES:.c=.o) $(ALLHSFILES:.hs=_stub.*))

clean	: mostlyclean
	$(strip $(RM) $(CHSFILES:.chs=.hs) $(CHSFILES:.chs=.chi) \
	  $(NEEDCHS:.chs=.hs) $(NEEDCHS:.chs=.chi) $(HSCFILES:.hsc=.hs)\
	  $(EXTRA_CLEANFILES))

distclean : clean
	$(strip $(RM) $(EXTRA_HSFILES) $(EXTRA_CHSFILES))

maintainer-clean : distclean




