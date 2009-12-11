all: Gen
	./bench.sh

Gen: Gen.hs
	ghc --make -O Gen

clean:
	rm -f Gen.hi Gen.o Gen
	rm -rf big
	rm -f *.data
