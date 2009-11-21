
PROG  = Embedded
SOURCES = Embedded.hs Event.hs Key.hs

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS) -XForeignFunctionInterface

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
