ALL = public/application.js public/stylesheet.css

.PHONY: all
all: $(ALL)

.PHONY: clean
clean:
	rm -f $(ALL)

public/application.js: $(wildcard src/* src/**/*)
	elm make --yes --output=$@ src/Main.elm

public/stylesheet.css: src/Stylesheet.elm src/Styles.elm
	elm css --module=Stylesheet $<
