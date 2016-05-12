PRODUCTION_FILES = $(shell find client/src -name '*.elm')
PRODUCTION_NATIVE_FILES = client/src/Native/Moment.js
PRODUCTION_OUTPUT_FILES = client/build/src/index.html client/build/src/stylesheet.css \
                          client/build/src/application.js client/build/src/load.js
TEST_FILES = client/test/run.js $(shell find client/test -name '*.elm')
TEST_NATIVE_FILES = client/test/Native/TestFramework.js
TEST_OUTPUT_FILES = client/build/test/test.js

.PHONY: build
build: $(PRODUCTION_OUTPUT_FILES)

.PHONY: test
test: client/build/test/run.js client/build/test/test.js
	node $<

.PHONY: serve
serve: build
	cd client/build/src && python3 -m http.server 8001

.PHONY: clean
clean:
	rm -f $(PRODUCTION_OUTPUT_FILES) $(PRODUCTION_NATIVE_FILES) $(TEST_OUTPUT_FILES) $(TEST_NATIVE_FILES)

client/build:
	mkdir -p $@

client/build/src/index.html: client/static/index.html client/build
	cp -f $< $@

client/build/src/load.js: client/static/load.js client/build
	cp -f $< $@

client/build/src/application.js: elm-package.json $(PRODUCTION_FILES) $(PRODUCTION_NATIVE_FILES) client/build
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES)

client/build/src/stylesheet.css: elm-package.json client/src/stylesheet.scss client/build
	./node_modules/.bin/node-sass client/src/stylesheet.scss > $@

client/build/test/run.js: client/test/run.js client/build
	cp -f $< $@

client/build/test/test.js: $(PRODUCTION_FILES) $(PRODUCTION_NATIVE_FILES) $(TEST_FILES) $(TEST_NATIVE_FILES)
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES) $(TEST_FILES)

client/src/Native:
	mkdir -p $@

client/src/Native/Moment.js: client/native/src/Moment.js client/src/Native
	./node_modules/.bin/browserify --outfile=$@ $<

client/test/Native:
	mkdir -p $@

client/test/Native/TestFramework.js: client/native/test/TestFramework.js client/test/Native
	./node_modules/.bin/browserify --outfile=$@ $<
