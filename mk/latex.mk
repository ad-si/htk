
# Rules to handle LaTex sources
#
# Strangely, this may not work with Solaris make-- use GNU make instead.

.SUFFIXES: .tex .bib .fig .eps .png .bbl .ps .pdf .dvi

FIG2DEV  = fig2dev
LATEX    = latex
PDFLATEX = pdflatex
BIBTEX   = bibtex
DVIPS    = dvips
PNGTOPNM = pngtopnm
PNMTOPS  = pnmtops

.fig.eps:
	$(FIG2DEV) -L ps $< > $@

.fig.pdf:
	$(FIG2DEV) -L pdf $< > $@

# This will fall over if there are too many colours in the PNG file,
# and it sometimes rotates the picture -- useless.
# .png.eps:
#	$(PNGTOPNM) $< | $(PNMTOPS) > $@

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

