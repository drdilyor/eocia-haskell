.PHONY: FORCE clean
all: output/a
output/a: output/a.o output/runtime.o
	gcc -o $@ $^
output/a.o: output/a.s
	as -msyntax=intel -mnaked-reg -o $@ $<
output/a.s: output/a.code FORCE
	cabal build exe:eocia-haskell
	cabal run   exe:eocia-haskell > $@ < output/a.code
output/runtime.o: output/runtime.c
	gcc -std=c99 -o $@ -c $<
clean:
	rm -f output/main output/main.o output/main.s output/runtime.o
FORCE:
