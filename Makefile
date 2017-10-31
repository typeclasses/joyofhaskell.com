build: stack-build
	stack exec -- site build

stack-build:
	stack build --fast

watch: stack-build
	stack exec -- site watch

clean: stack-build
	stack exec -- site clean

deploy: clean build
	rsync --verbose --recursive --compress _site/ deploy@joyofhaskell.com:/home/deploy/web/

.PHONY: build stack-build watch clean deploy
