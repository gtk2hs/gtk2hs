PROGS  = Embedded Uzbl
SOURCES = Embedded.hs Uzbl.hs

all : $(PROGS)

Embedded : Embedded.hs
	$(HC_RULE)

Uzbl : Uzbl.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
