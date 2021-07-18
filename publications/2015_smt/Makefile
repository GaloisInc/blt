FIGURES=figures/solcount2.pdf figures/blt_benchmark.pdf
SECTIONS=introduction.tex bcp.tex decision-procedure.tex jpeg.tex related.tex

blt-paper.pdf : blt-paper.tex $(SECTIONS) blt-paper.bib $(FIGURES)
	xelatex blt-paper
	bibtex  blt-paper
	xelatex blt-paper
	xelatex blt-paper

distclean : clean
	rm -f blt-paper.pdf

clean :
	rm -f *.aux *.bbl *.blg *.log *.out *.toc

.PHONY : clean distclean
