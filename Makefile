.PHONY: clean run
.SUFFIXES:

build:
	ghc -O2 Tracer.hs

run:
	time ./Tracer | feh -

clean:
	rm *.o *.hi
