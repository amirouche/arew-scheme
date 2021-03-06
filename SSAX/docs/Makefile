#	    Generating documentation from SXML code
#
# The targets to make:
#	SXML.html (implicit)    Make the SXML Specification in HTML
#	SXML-paper.tex          Make the SXML Specification in TeX
#	SXs.tex			Make the SXSLT/SXPath paper in LaTeX

# $Id: Makefile,v 3.0 2004/02/19 22:08:48 oleg Exp $

# Directory where all the "included" modules are located
LIB=../lib

# Converter function. We are using Bigloo at present
CONV=bigloo -i -s -eval "(module html (include \"$(LIB)/myenv-bigloo.scm\") (include \"$(LIB)/srfi-13-local.scm\") (include \"$(LIB)/util.scm\") (include \"$(LIB)/SXML-tree-trans.scm\") (include \"$(LIB)/SXML-to-HTML.scm\") (include \"$(LIB)/SXML-to-HTML-ext.scm\"))"

# Convert SXML to HTML
%.html: %.scm
	$(CONV) $*.scm > $@

# Convert SXML to LaTeX
%.tex: %.scm
	$(CONV) $*.scm > $@



SXML-paper.tex: SXML-paper.scm SXML.scm
	$(CONV) SXML-paper.scm > SXML-paper.tex
#	latex SXML-paper.tex

clean:
	rm -f *_.c *.o core a.out 

