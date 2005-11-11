
PROGS  = drawing drawing2 starandring text
SOURCES = Drawing.hs Drawing2.hs StarAndRing.hs Text.hs
PACKAGES = gtk cairo

all : $(PROGS)

drawing : Drawing.hs
	$(HC_RULE)

drawing2 : Drawing2.hs
	$(HC_RULE)

starandring : StarAndRing.hs
	$(HC_RULE)

text : Text.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)
	rm -f *.png

HC=ghc
