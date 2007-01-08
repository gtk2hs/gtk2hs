
PROGS  = listdemo treedemo listtest treetest dirlist
SOURCES = ListDemo.hs TreeDemo.hs ListTest.hs TreeTest.hs DirList.hs
PACKAGES = glade

all : $(PROGS)

listdemo : ListDemo.hs
	$(HC_RULE)

treedemo : TreeDemo.hs
	$(HC_RULE)

listtest : ListTest.hs
	$(HC_RULE)

treetest : TreeTest.hs
	$(HC_RULE)

dirlist : DirList.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
