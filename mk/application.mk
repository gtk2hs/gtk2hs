TARGETOK		= $(addsuffix $(APPSUFFIX),$(strip $(APPNAME)))

$(TARGETOK) : inplaceinit $(ALLHSFILES) $(EXTRA_CFILES:.c=.o)
	$(RM) $@
	$(strip $(HC) --make $(MAINOK) -o $@ $(HCINCLUDES) \
	  -package-conf $(LOCALPKGCONF) \
	  $(EXTRA_LIBS_ONLY_L)  $(EXTRA_CPPFLAGS_ONLY_I) $(HC_FLAGS) \
	  $(LIBS_ONLY_L) $(CPPFLAGS_ONLY_I) \
	  $(EXTRAHC_FLAGS) -i$(HIDIRSOK) $(NEEDPACKAGESOK) $(STUBOFILES) \
	  $(EXTRA_CFILES:.c=.o) $(addprefix -#include ,$(STUBHFILES)))

EXTRA_CLEANFILES += Main.hi $(TARGETOK)

install-without-pkg: install
