.PHONY: build
build:
	make -C dashboard
	make -C database
	touch .built

.PHONY: check
check:
	make -C dashboard check

.PHONY: clean
clean:
	make -C dashboard clean
