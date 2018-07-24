paper.pdf: *.tex *.png *.sty
	xelatex paper.tex
	bibtex paper
	xelatex paper.tex
	xelatex paper.tex

clean:
	rm *.out *.pdf *.aux *.log 

.PHONY: clean
