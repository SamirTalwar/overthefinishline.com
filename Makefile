.PHONY: build
build:
	make -C dashboard
	make -C database
	make -C proxy
	touch .built

.PHONY: check
check:
	make -C dashboard check

.PHONY: test
test:
	make -C dashboard test

.PHONY: push
push: build test
	make -C dashboard push
	make -C proxy push

.PHONY: clean
clean:
	make -C dashboard clean
