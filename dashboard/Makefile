PRODUCTION_FILES = $(shell find client/src -name '*.elm')
PRODUCTION_NATIVE_FILES = client/src/Native/Moment.js
PRODUCTION_OUTPUT_FILES = client/build/src/index.html client/build/src/stylesheet.css \
                          client/build/src/application.js client/build/src/load.js
TEST_FILES = $(shell find client/test -name '*.elm')

TAG = samirtalwar/overthefinishline.com-dashboard

.PHONY: build
build: $(PRODUCTION_OUTPUT_FILES) $(PRODUCTION_NATIVE_FILES)
	stack image container

.PHONY: check
check: test lint

.PHONY: test
test: $(PRODUCTION_OUTPUT_FILES) $(PRODUCTION_NATIVE_FILES)
	node ./elm-stuff/packages/SamirTalwar/arborist/3.0.0/bin/run Tests.tests $(TEST_FILES)
	stack runhaskell -- -iserver/app:server/src:server/test server/test/Spec.hs

.PHONY: lint
lint:
	stack exec which hlint >/dev/null || stack install hlint
	stack exec hlint -- --ignore='Use ***' .

.PHONY: push
push: build test lint
	docker push $(TAG)

.PHONY: clean
clean:
	rm -rf client/build $(PRODUCTION_NATIVE_FILES)
	rm -rf elm-stuff/build-artifacts

client/build:
	mkdir -p $@

client/build/src:
	mkdir -p $@

client/build/src/index.html: client/static/index.html client/build/src
	cp -f $< $@

client/build/src/load.js: client/static/load.js client/build/src
	cp -f $< $@

client/build/src/application.js: elm-package.json $(PRODUCTION_FILES) $(PRODUCTION_NATIVE_FILES) client/build/src
	elm make --warn --yes --output=$@ $(PRODUCTION_FILES)

client/build/src/stylesheet.css: elm-package.json client/src/stylesheet.scss client/build/src node_modules
	./node_modules/.bin/node-sass client/src/stylesheet.scss > $@

client/src/Native:
	mkdir -p $@

client/src/Native/Moment.js: client/native/src/Moment.js client/src/Native node_modules
	./node_modules/.bin/browserify --outfile=$@ $<

node_modules: package.json
	npm install && npm prune
