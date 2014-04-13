all: Testcalc Doccalc.ps

Testcalc: Testcalc.hs Parcalc.hs Lexcalc.hs
	ghc --make Testcalc.hs -o Testcalc

Parcalc.hs: Parcalc.y
	happy -gca Parcalc.y
	
Lexcalc.hs: Lexcalc.x
	alex -g Lexcalc.x

Doccalc.dvi: Doccalc.tex
	latex Doccalc.tex

Doccalc.ps: Doccalc.dvi
	dvips Doccalc.dvi -o Doccalc.ps

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doccalc.ps
distclean: clean
	-rm -f Doccalc.* Lexcalc.* Parcalc.* Layoutcalc.* Skelcalc.* Printcalc.* Testcalc.* Abscalc.* Testcalc ErrM.* SharedString.* calc.dtd XMLcalc.* 

Abscalc.hs: bnf
Doccalc.tex: bnf
Doccalc.txt: bnf
ErrM.hs: bnf
Lexcalc.x: bnf
Parcalc.y: bnf
Printcalc.hs: bnf
Skelcalc.hs: bnf
Testcalc.hs: bnf

bnf: calc.cf
	bnfc -haskell calc.cf

.PHONY: bnf clean all
