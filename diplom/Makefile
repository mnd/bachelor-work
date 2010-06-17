all: thesis.tex title.tex first.tex second.tex third.tex refs.bib intro.tex concl.tex appendix.tex refer.tex
	latex thesis
	bibtex thesis
	latex thesis
	latex thesis
	pdflatex thesis

clean:
	# rm -f *.pdf
	# rm -f *.dvi
	rm -f *.log
	rm -f *.aux
	rm -f *~
	rm -f *.bbl
	rm -f *.blg
	rm -f *.nlo
	rm -f *.toc
	rm -f *.out