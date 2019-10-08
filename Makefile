all: untitled untitled.pdf

untitled: untitled.tex
	ghc Untitled.hs Main.hs -o untitled

untitled.tex: Untitled.hs
	./hs2tex Untitled.hs untitled.tex

untitled.pdf: untitled.tex
	git log | head  -n 1 | cut -d ' ' -f 2   > version.tex
	pdflatex untitled.tex
	pdflatex untitled.tex
