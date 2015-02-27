

.PHONY = mhrtproc

mhrtproc: bin/mhrtproc

bin/%: %.hs
	ghc -o $@ --make $^
