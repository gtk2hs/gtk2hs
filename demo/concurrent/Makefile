
PROGS  = progress progressThreadedRTS
SOURCES = Progress.hs ProgressThreadedRTS.hs

all : $(PROGS)

progress : Progress.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

progressThreadedRTS : ProgressThreadedRTS.hs
	$(HC) --make $< -o $@ -threaded $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
