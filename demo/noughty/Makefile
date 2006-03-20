
PROGS  = noughty noughty-glade
SOURCES = Noughty.hs NoughtyGlade.hs
PACKAGES = gtk glade

all : $(PROGS)

noughty : Noughty.hs
	$(HC_RULE)

noughty-glade : NoughtyGlade.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
