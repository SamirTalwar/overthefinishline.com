INPUT_FILES = $(shell find app src -name '*.hs')

.PHONY: build
build: $(INPUT_FILES)
	stack build

.PHONY: test
test: build
	stack test
