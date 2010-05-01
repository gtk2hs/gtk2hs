
PROGS  = test-sync test-dir test-xfer test-drive-volume test-volume-monitor
SOURCES = TestSync.hs TestDir.hs TestXfer.hs TestDriveVolume.hs TestVolumeMonitor.hs

all: $(PROGS)

test-sync : TestSync.hs
	$(HC_RULE)
test-dir : TestDir.hs
	$(HC_RULE)
test-xfer : TestXfer.hs
	$(HC_RULE)
test-drive-volume : TestDriveVolume.hs
	$(HC_RULE)
test-volume-monitor : TestVolumeMonitor.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
