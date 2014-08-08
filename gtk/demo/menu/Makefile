
PROGS  = menudemo combodemo
SOURCES = MenuDemo.hs ComboDemo.hs

all : $(PROGS)

menudemo : MenuDemo.hs
	$(HC_RULE)

combodemo : ComboDemo.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
