
# Rules to handle LaTex sources
#
# Strangely, this may not work with Solaris make-- use GNU make instead.

.SUFFIXES: .tex .bib .fig .eps .bbl .ps .pdf .dvi

FIG2DEV = fig2dev
LATEX   = latex
PDFLATEX = pdflatex
BIBTEX  = bibtex
DVIPS   = dvips

.fig.eps:
	$(FIG2DEV) -L ps $< > $@

.fig.pdf:
	$(FIG2DEV) -L pdf $< > $@

.dvi.ps:
	$(DVIPS) -o $@ $<

.tex.pdf: 
	$(PDFLATEX) $<; $(PDFLATEX) $<

# This will fall over if we did not run LaTeX on $< at least once to
# create a .aux file...
.bib.bbl:
	$(LATEX) $*.tex; $(BIBTEX) $*

.tex.dvi:
	$(LATEX) $<; $(LATEX) $<

#

