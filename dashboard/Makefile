ALL = public/application.js public/stylesheet.css

.PHONY: all
all: $(ALL)

.PHONY: clean
clean:
	rm -f $(ALL)

public/application.js: $(wildcard src/*.elm src/**/*.elm)
	elm make --warn --yes --output=$@ $^

public/stylesheet.css: src/stylesheet.scss
	./node_modules/.bin/node-sass $< > $@
