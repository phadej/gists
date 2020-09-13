PDFS := Squash ColorsInLhs2TeX LatexSVG

all : images pdfs
	cabal v2-build gists:site
	`cabal-plan list-bin site` rebuild
	@echo file://`pwd`/_site/index.html

pdfs : $(PDFS:%=pdf/%.pdf)

images : 
	mkdir -p images
	$(MAKE) -C images-src
	cp images-src/*.png images
	cp images-src/*.svg images
	cp images-static/*.png images
	cp images-static/*.svg images

repl:
	cabal v2-repl gists-runnable:lib:gists-literate

watch :
	cabal v2-build gists:site
	GISTS_RECENT=0 `cabal-plan list-bin site` watch

clean :
	cabal v2-build gists:site
	`cabal-plan list-bin site` clean

upload : all
	dotenv -f .env -- rsync -4 -avz --progress _site/ '$$PRODUCTION'

preview : all
	dotenv -f .env -- rsync -4 -avz --progress _site/ '$$STAGING'

tmp/%.tex : pkg/literate/%.lhs
	mkdir -p tmp
	lhs2TeX $< > $@

pdf/%.pdf : tmp/%.tex preamble.tex
	mkdir -p pdf
	pdflatex -draftmode -halt-on-error -output-directory=tmp $<
	pdflatex -output-directory=tmp $<
	cp tmp/$*.pdf pdf/$*.pdf
