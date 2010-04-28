
PROGS  = webkit
SOURCES = Webkit.hs

all : $(PROGS)

webkit : Webkit.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
