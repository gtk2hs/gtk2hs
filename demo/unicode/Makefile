
PROG  = arabic
SOURCES = Arabic.hs

$(PROG) : $(SOURCES)
	ghc --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
