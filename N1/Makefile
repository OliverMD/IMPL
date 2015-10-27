all:
	happy -gca ParRules.y
	alex -g LexRules.x
	ghc --make TestRules.hs -o TestRules

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocRules.* LexRules.* ParRules.* LayoutRules.* SkelRules.* PrintRules.* TestRules.* AbsRules.* TestRules ErrM.* SharedString.* ComposOp.* rules.dtd XMLRules.* Makefile*
	

