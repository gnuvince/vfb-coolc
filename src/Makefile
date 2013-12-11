BUILDDIR=_build

lexer: Lexer.x
	mkdir -p $(BUILDDIR)
	alex -o $(BUILDDIR)/Lexer.hs Lexer.x
	ghc --make -O2 -outputdir $(BUILDDIR) $(BUILDDIR)/Lexer.hs
