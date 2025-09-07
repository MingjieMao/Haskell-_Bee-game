beesimulate: Main.hs $(wildcard *.hs)
	ghc -o $@ $<

.PHONY: clean
clean:
	-rm -f *.o *.hi beesimulate