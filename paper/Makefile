paper.pdf: paper.scrbl
	scribble --latex paper.scrbl
	xelatex -interaction nonstopmode -file-line-error paper.tex

paper.html: paper.scrbl
	scribble paper.scrbl

clean:
	rm *.css *.js *.tex *.out *.pdf *.aux *.cls *.html

.PHONY: clean
