
PROG  = profileviewer
SOURCES = ProfileViewer.hs ParseProfile.hs
PACKAGES = glade

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
