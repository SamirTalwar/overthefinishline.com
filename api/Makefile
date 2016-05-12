INPUT_FILES = $(shell find app src -name '*.hs')

.PHONY: build
build: $(INPUT_FILES)
	stack build

.PHONY: test
test: build
	stack test

.PHONY: serve
serve: build
	stack exec overthefinishline-api-exe 8002
