all: Untitled.html untitled

untitled: Untitled.hs
	ghc Untitled.hs Main.hs -o untitled



Untitled.html: Untitled.adoc
	asciidoc -b html5 -a icons -a toc2 -a theme=flask -n Untitled.adoc


Untitled.adoc:  Untitled.hs
	sed -e 's/^{-//g' Untitled.hs > Untitled.adoc
	sed -i -e 's/\-}//g' Untitled.adoc
