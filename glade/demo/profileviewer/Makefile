
PROG  = profileviewer
SOURCES = ProfileViewer.hs ParseProfile.hs

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
