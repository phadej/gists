all : Monotone.pdf HigherOrderRoles.pdf

%.pdf : %.tex
	latexmk -interaction=nonstopmode -pdf -no-shell-escape -bibtex $<

%.tex : src/%.lhs
	lhs2TeX $< > $@

build :
	cabal v2-build libs

run :
	cabal v2-run monotone
