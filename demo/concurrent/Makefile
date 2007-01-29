
PROGS  = progress progressThreadedRTS
SOURCES = Progress.hs ProgressThreadedRTS.hs
PACKAGES = gtk

all : $(PROGS)

progress : Progress.hs
	$(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

progressThreadedRTS : ProgressThreadedRTS.hs
	$(HC) --make $< -o $@ -threaded $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
