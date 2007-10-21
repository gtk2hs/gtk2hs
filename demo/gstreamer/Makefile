
PROGS  = vorbis-play
SOURCES = VorbisPlay.hs

all: $(PROGS)

vorbis-play : VorbisPlay.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
