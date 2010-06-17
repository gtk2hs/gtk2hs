
PROGS  = Embedded Uzbl MPlayer
SOURCES = Embedded.hs Uzbl.hs MPlayer.hs

all : $(PROGS)

Embedded : Embedded.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

Uzbl : Uzbl.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

MPlayer : MPlayer.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
