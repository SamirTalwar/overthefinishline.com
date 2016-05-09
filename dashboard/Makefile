ALL = \
	public/application.js public/stylesheet.css public/vendor/bootstrap

.PHONY: all
all: $(ALL)

.PHONY: clean
clean:
	rm -f $(ALL)

public/application.js: $(wildcard src/* src/**/*)
	elm make --yes --output=$@ src/Main.elm

public/stylesheet.css: src/Stylesheet.elm src/Styles.elm
	elm css --module=Stylesheet $<

public/vendor/bootstrap: \
		vendor/bootstrap/node_modules vendor/bootstrap/node_modules/.bin/grunt
	cd vendor/bootstrap \
		&& ./node_modules/.bin/grunt dist \
		&& mkdir -p ../../public/vendor \
		&& rm -rf ../../public/vendor/bootstrap \
		&& cp -R dist ../../public/vendor/bootstrap \
		&& git checkout dist docs/assets

vendor/bootstrap/node_modules: vendor/bootstrap/package.json
	cd vendor/bootstrap \
		&& npm install

vendor/bootstrap/node_modules/.bin/grunt:
	cd vendor/bootstrap \
		&& npm install grunt-cli
