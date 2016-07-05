HFLAGS=-fPIC -dynamic -Wall

all:
	ghc --make ${HFLAGS} Main.hs -o dispatch-merge

clean:
	rm -f *.hi *.o dispatch-merge

test:
	./dispatch-merge test.txt
