.PHONY: clean run
.SUFFIXES:

build:
	ghc -O2 Tracer.hs

run: build
	time ./Tracer | feh -

clean:
	rm *.o *.hi
