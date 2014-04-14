all: Lang/TestCalc Calc.ps Lang/ConvertCalcAbsToLLVM

Lang/ConvertCalcAbsToLLVM: Lang/ConvertCalcAbsToLLVM.hs Lang/AbsCalc.hs Lang/PromoteNum.hs Lang/AbsCalc.hs
	ghc -main-is Lang/ConvertCalcAbsToLLVM Lang/ConvertCalcAbsToLLVM.hs

Lang/TestCalc: Lang/TestCalc.hs Lang/ParCalc.hs Lang/LexCalc.hs
	ghc --make Lang/TestCalc.hs -o Lang/TestCalc
Lang/TestPromote: Lang/TestPromote.hs Lang/ParCalc.hs Lang/LexCalc.hs
	ghc --make Lang/TestPromote.hs -o Lang/TestCalc

Lang/ParCalc.hs: Lang/ParCalc.y
	happy -gca Lang/ParCalc.y
	
Lang/LexCalc.hs: Lang/LexCalc.x
	alex -g Lang/LexCalc.x

Calc.dvi: Calc.tex
	latex Calc.tex

Calc.ps: Calc.dvi
	dvips Calc.dvi -o Calc.ps

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Lang/*.log Lang/*.aux Lang/*.hi Lang/*.o Lang/*.dvi
	-rm -f Calc.ps
	-rm -f Lang/ConvertCalcAbsToLLVM

distclean: clean
	-rm -f Lang/Calc.* Lang/LexCalc.* Lang/ParCalc.* Lang/LayoutCalc.* Lang/SkelCalc.* Lang/PrintCalc.* Lang/TestCalc.* Lang/AbsCalc.* Lang/TestCalc Lang/ErrM.* Lang/SharedString.* Lang/Calc.dtd Lang/XMLCalc.* 
	-rm -f Lang/ConvertCalcAbsToLLVM

Lang/AbsCalc.hs: bnf
Calc.tex: bnf
Lang/Calc.txt: bnf
Lang/ErrM.hs: bnf
Lang/LexCalc.x: bnf
Lang/ParCalc.y: bnf
Lang/PrintCalc.hs: bnf
Lang/SkelCalc.hs: bnf
Lang/TestCalc.hs: bnf

bnf: Calc.cf
	bnfc --haskell --ghc -p Lang Calc.cf
	bnfc --latex Calc.cf

.PHONY: bnf clean all
