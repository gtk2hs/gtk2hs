
PROGS  = bouncingball snowflake demo1 demo2

SOURCES = BouncingBall.hs Demo1.hs Demo2.hs Snowflake.hs
PACKAGES = gtk soegtk

all : $(PROGS)

bouncingball : BouncingBall.hs
	$(HC_RULE)

snowflake : Snowflake.hs
	$(HC_RULE)

demo1 : Demo1.hs
	$(HC_RULE)

demo2 : Demo2.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS) $(HCEXTRAFLAGS)

HCEXTRAFLAGS = $(if $(HCNEEDSPACKAGE), $(addprefix -package ,$(PACKAGES)))

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
