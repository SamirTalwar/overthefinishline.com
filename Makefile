.PHONY: build
build:
	make -C dashboard
	make -C database
	touch .built

.PHONY: test
test:
	make -C dashboard test
