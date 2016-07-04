all:
	ghc --make -fPIC -dynamic Main.hs -o dispatch-merge

clean:
	rm -f *.hi *.o dispatch-merge

