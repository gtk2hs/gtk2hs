
PROG  = gconfdemo
SOURCES = GConfDemo.hs
PACKAGES = gconf

$(PROG) : $(SOURCES)
	ghc --make $< -o $@ -fglasgow-exts -fallow-overlapping-instances $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
