
PROG  = calc
SOURCES = Calc.hs CalcModel.hs
PACKAGES = glade

$(PROG) : $(SOURCES)
	ghc --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
