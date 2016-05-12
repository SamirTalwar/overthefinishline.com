PRODUCTION_FILES = $(shell find src -name '*.elm')
PRODUCTION_NATIVE_FILES = src/Native/Moment.js
PRODUCTION_OUTPUT_FILES = public/application.js public/stylesheet.css
TEST_FILES = $(shell find test -name '*.elm')
TEST_NATIVE_FILES = test/Native/TestFramework.js
TEST_OUTPUT_FILES = elm-stuff/test.js

.PHONY: build
build: $(PRODUCTION_OUTPUT_FILES)

.PHONY: test
test: elm-stuff/test.js
	node test/run.js

.PHONY: serve
serve: build
	cd public && python3 -m http.server 8001

.PHONY: clean
clean:
	rm -f $(PRODUCTION_OUTPUT_FILES) $(PRODUCTION_NATIVE_FILES) $(TEST_OUTPUT_FILES) $(TEST_NATIVE_FILES)

elm-stuff/test.js: test/run.js $(PRODUCTION_FILES) $(PRODUCTION_NATIVE_FILES) $(TEST_FILES) $(TEST_NATIVE_FILES)
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES) $(TEST_FILES)

public/application.js: elm-package.json $(PRODUCTION_FILES) $(PRODUCTION_NATIVE_FILES)
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
