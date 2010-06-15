all: thesis.tex title.tex first.tex second.tex third.tex refs.bib
	latex thesis
	bibtex thesis
	latex thesis
	latex thesis

clean:
	rm *.pdf
	rm *.log
	rm *.aux
	rm *~
	rm *.bbl
	rm *.blg
	rm *.nlo
	rm *.toc