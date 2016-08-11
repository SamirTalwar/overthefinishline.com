.PHONY: build
build:
	make -C dashboard
	touch .built

.PHONY: test
test:
	make -C dashboard test
