all: thesis presentation

thesis: thesis.tex title.tex first.tex second.tex third.tex refs.bib intro.tex concl.tex appendix.tex refer.tex
	latex thesis
	bibtex thesis
	latex thesis
	latex thesis
	pdflatex thesis

presentation: presentation.tex
	pdflatex presentation
	pdflatex presentation

clean:
	# rm -f *.pdf *.dvi
	rm -f *.log *.aux *~ *.bbl *.blg *.nlo *.toc *.out *.nav *.snm
