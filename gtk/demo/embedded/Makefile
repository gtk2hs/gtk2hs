
PROGS  = Embedded Uzbl
SOURCES = Embedded.hs Uzbl.hs

all : $(PROGS)

Embedded : Embedded.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

Uzbl : Uzbl.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
