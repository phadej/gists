Monotone.pdf : Monotone.tex lattice-bool-to-m2.png
	latexmk -interaction=nonstopmode -pdf -no-shell-escape -bibtex Monotone.tex

Monotone.tex : src/Monotone.lhs
	lhs2TeX src/Monotone.lhs > Monotone.tex

lattice-bool-to-m2.png : lattice-bool-to-m2.dot
	cat lattice-bool-to-m2.dot | dot -Tpng -olattice-bool-to-m2.png

run : 
	cabal v2-run monotone
