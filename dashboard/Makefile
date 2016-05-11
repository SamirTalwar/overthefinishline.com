OUTPUT_FILES = public/application.js public/stylesheet.css
PRODUCTION_FILES = $(shell find src -name '*.elm')
NATIVE_PRODUCTION_FILES = src/Native/Moment.js
TEST_FILES = $(shell find test -name '*.elm')
NATIVE_TEST_FILES = test/Native/TestFramework.js

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
	node test/run.js

elm-stuff/test.js: test/run.js $(PRODUCTION_FILES) $(NATIVE_PRODUCTION_FILES) $(TEST_FILES) $(NATIVE_TEST_FILES)
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES) $(TEST_FILES)

public/application.js: elm-package.json $(PRODUCTION_FILES) $(NATIVE_PRODUCTION_FILES)
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES)

public/stylesheet.css: elm-package.json src/stylesheet.scss
	./node_modules/.bin/node-sass src/stylesheet.scss > $@

src/Native:
	mkdir -p src/Native

src/Native/Moment.js: native/Moment.js src/Native
	./node_modules/.bin/browserify --outfile=$@ $<

test/Native:
	mkdir -p test/Native

test/Native/TestFramework.js: native/TestFramework.js test/Native
	./node_modules/.bin/browserify --outfile=$@ $<
