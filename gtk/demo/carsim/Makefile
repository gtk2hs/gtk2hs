
PROGS  = carsim
SOURCES = CarSim.hs

all : $(PROGS)

carsim : CarSim.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
