build: stack-build
	stack exec -- site build

stack-build:
	stack build --fast

watch: stack-build
	stack exec -- site watch

clean: stack-build
	stack exec -- site clean

deploy: clean build
	aws s3 sync _site s3://joh-web-public-1 --delete --profile joyofhaskell

.PHONY: build stack-build watch clean deploy
