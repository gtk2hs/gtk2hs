
PROGS  = simplelist listtest listtest2 treetest
SOURCES = SimpleList.hs ListTest.hs ListTest2.hs TreeTest.hs
PACKAGES = glade

all : $(PROGS)

simplelist : SimpleList.hs
	$(HC_RULE)

listtest : ListTest.hs
	$(HC_RULE)

listtest2 : ListTest2.hs
	$(HC_RULE)

treetest : TreeTest.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
