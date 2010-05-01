
PROGS  = svg2png svgviewer
SOURCES = Svg2Png.hs SvgViewer.hs

all : $(PROGS)

svg2png : Svg2Png.hs
	$(HC_RULE)

svgviewer : SvgViewer.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)
	rm -f *.png

HC=ghc
