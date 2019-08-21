# HC=/opt/ghc/7.10.3/bin/ghc
# HC=ghc
HC=stack ghc --
SOURCES=Main.hs src/Lattice.hs src/Utility.hs
PACKAGE=hw4.zip

.PHONY: pack all run clean

all: lattice

run: lattice
	./lattice

clean:
	rm -rf src/*.o src/*.hi
	rm -f lattice

lattice: $(SOURCES)
	$(HC) -O3 -i./src -tmpdir . ./Main.hs -o lattice

pack:
	zip $(PACKAGE) -r Makefile src Main.hs
