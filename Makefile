HFLAGS=-fPIC -dynamic -Wall

dispatch-merge: *.hs
	ghc --make ${HFLAGS} Main.hs -o dispatch-merge

clean:
	rm -f *.hi *.o dispatch-merge

test: dispatch-merge
	./dispatch-merge test/test.txt
