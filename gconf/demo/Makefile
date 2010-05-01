
PROG  = gconfdemo
SOURCES = GConfDemo.hs

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ -fglasgow-exts -fallow-overlapping-instances $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
