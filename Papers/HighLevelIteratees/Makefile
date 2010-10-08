all: HighLevelIteratees.pdf HighLevelIteratees.o HighLevelEnumerators.o

typecheck: HighLevelIteratees.o HighLevelEnumerators.o

HighLevelIteratees.pdf: HighLevelIteratees.tex subtitle.sty
	pdflatex HighLevelIteratees.tex && pdflatex HighLevelIteratees.tex
	$(MAKE) clean_tempfiles

HighLevelIteratees.o: HighLevelIteratees.lhs
	ghc --make -O2 HighLevelIteratees

HighLevelEnumerators.o: HighLevelEnumerators.lhs HighLevelIteratees.lhs
	ghc --make -O2 HighLevelEnumerators

HighLevelIteratees.tex: HighLevelIteratees.lhs HighLevelEnumerators.lhs HighLevelIteratees.fmt
	lhs2tex --poly HighLevelIteratees.lhs > HighLevelIteratees.tex

clean_tempfiles:
	rm -f HighLevelIteratees.aux HighLevelIteratees.log HighLevelIteratees.ptb

clean: clean_tempfiles
	rm -f HighLevelIteratees.tex HighLevelIteratees.pdf
	rm -f *.hi *.o
