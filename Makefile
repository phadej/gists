all : images
	cabal new-build gists:site
	`cabal-plan list-bin site` rebuild
	@echo file://`pwd`/_site/index.html

images : 
	mkdir -p images
	$(MAKE) -C images-src
	cp images-src/*.png images
	cp images-src/*.svg images
	cp images-static/*.png images
	cp images-static/*.svg images

repl:
	cabal new-repl gists-runnable

watch :
	cabal new-build gists:site
	`cabal-plan list-bin site` watch

clean :
	cabal new-build gists:site
	`cabal-plan list-bin site` clean

upload : all
	dotenv -f .env -- rsync -avz --progress _site/ '$$PRODUCTION'

preview : all
	dotenv -f .env -- rsync -avz --progress _site/ '$$STAGING'
