default: alg01.pdf

alg01.pdf : alg01.tex
	pdflatex alg01.tex
	bibtex alg01
	pdflatex alg01.tex
	pdflatex alg01.tex
	pdflatex alg01.tex

.PHONY: clean

clean:
	rm -rf alg01.pdf *.aux *.lot *.lof *.out *.log *.bbl *.blg
