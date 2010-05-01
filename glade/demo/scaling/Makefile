
PROG  = scaling
SOURCES = Scaling.hs
#HCFLAGS = -prof -auto-all
# use -fglasgow-exts since older ghc versions don't know about FlexibleContexts
HCFLAGS = -O3 -fglasgow-exts
#HCFLAGS = -O3 -fvia-C -optc-O3
#HCFLAGS = -O0 -keep-hc-file -keep-s-files -fvia-C

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
