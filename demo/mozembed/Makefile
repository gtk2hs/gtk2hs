
PROG  = testembedmoz
SOURCES = TestEmbedMoz.hs
PACKAGES = mozembed

$(PROG) : $(SOURCES)
	ghc --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
