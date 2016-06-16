PRODUCTION_FILES = $(shell find client/src -name '*.elm')
PRODUCTION_NATIVE_FILES = client/src/Native/Moment.js
PRODUCTION_OUTPUT_FILES = client/build/src/index.html client/build/src/stylesheet.css \
                          client/build/src/application.js client/build/src/load.js
TEST_FILES = $(shell find client/test -name '*.elm')

.PHONY: build
build: $(PRODUCTION_OUTPUT_FILES)
	cd server && sbt assembly

.PHONY: test
test: $(PRODUCTION_FILES) $(PRODUCTION_NATIVE_FILES) $(TEST_FILES) node_modules
	./vendor/arborist/bin/run $(TEST_FILES)
	cd server && sbt test

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

client/build/src/stylesheet.css: elm-package.json client/src/stylesheet.scss client/build node_modules
	./node_modules/.bin/node-sass client/src/stylesheet.scss > $@

client/src/Native:
	mkdir -p $@

client/src/Native/Moment.js: client/native/src/Moment.js client/src/Native node_modules
	./node_modules/.bin/browserify --outfile=$@ $<

node_modules: package.json
	npm install
