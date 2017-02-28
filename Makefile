build: stack-build
	stack exec -- site build

stack-build:
	stack build --fast

watch: stack-build
	stack exec -- site watch

clean: stack-build
	stack exec -- site clean

deploy: clean build
	rsync -avz _site/ deploy@joyofhaskell.com:/home/deploy/web/ --port=36411

.PHONY: build stack-build watch clean deploy
