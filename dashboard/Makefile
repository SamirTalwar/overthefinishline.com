OUTPUT_FILES = public/application.js public/stylesheet.css
PRODUCTION_FILES = $(shell find src -name '*.elm')
TEST_FILES = $(shell find test -name '*.elm')

.PHONY: all
all: $(OUTPUT_FILES)

.PHONY: clean
clean:
	rm -f $(OUTPUT_FILES)

.PHONY: serve
serve:
	cd public && python3 -m http.server

.PHONY: test
test: elm-stuff/test.js
	node $^

elm-stuff/test.js: test/run.js $(PRODUCTION_FILES) $(TEST_FILES)
	elm make --warn --yes --output $@ $(PRODUCTION_FILES) $(TEST_FILES)
	cat $< >> $@

public/application.js: elm-package.json $(PRODUCTION_FILES)
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES)

public/stylesheet.css: elm-package.json src/stylesheet.scss
	./node_modules/.bin/node-sass src/stylesheet.scss > $@
