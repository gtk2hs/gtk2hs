
PROG  = sourceview
SOURCES = SourceViewTest.hs
PACKAGES = sourceview

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
