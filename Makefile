HFLAGS=-fPIC -dynamic -Wall -XTupleSections

dispatch-merge: *.hs
	ghc --make ${HFLAGS} Main.hs -o dispatch-merge -odir build -hidir build

clean:
	rm -f dispatch-merge build/*

test: dispatch-merge
	./dispatch-merge test/test.txt
