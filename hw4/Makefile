# HC=/opt/ghc/7.10.3/bin/ghc
HC=ghc
# SOURCES=Main.hs src/Grammar.hs src/Deduction.hs src/Proof.hs src/ProofGenerator.hs src/Utility.hs src/Annotation.hs TODO
# GEN_SOURCES=src/Lexer.x src/Parser.y TODO
# GENERATED=src/Lexer.hs src/Parser.hs TODO
PACKAGE=hw4.zip

.PHONY: pack all run clean

all: parser

run: parser
	./parser

clean:
	rm -rf src/*.o src/*.hi
	# rm -rf $(GENERATED)
	rm -f parser

parser: $(GENERATED) $(SOURCES)
	$(HC) -O3 -i./src -tmpdir . ./Main.hs -o parser

$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src Main.hs
