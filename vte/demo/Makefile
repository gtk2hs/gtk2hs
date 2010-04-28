
PROG  = vte 
SOURCES = Vte.hs

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS) -XForeignFunctionInterface

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
