build: stack-build
	stack exec -- site build

stack-build:
	stack build --fast

watch: stack-build
	stack exec -- site watch

clean: stack-build
	stack exec -- site clean

deploy: clean build
	rsync -avz -e 'ssh -p 36411' _site/ deploy@joyofhaskell.com:/home/deploy/web/

.PHONY: build stack-build watch clean deploy
