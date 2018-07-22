paper.pdf: paper.tex
	xelatex paper.tex
	bibtex paper
	xelatex paper.tex
	xelatex paper.tex

clean:
	rm *.out *.pdf *.aux *.log 

.PHONY: clean
